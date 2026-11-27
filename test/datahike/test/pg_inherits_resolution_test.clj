(ns datahike.test.pg-inherits-resolution-test
  "Ordinary table inheritance through SQL, including child column resolution,
   physical relation keys, and source-linked PostgreSQL 18 regression cases."
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

(defn- execute ^PgWireServer$QueryResult [sql]
  (.execute *handler* sql))

(defn- exec! [sql]
  (let [result (execute sql)]
    (is (nil? (.-error result)) sql)))

;; ---------------------------------------------------------------------------
;; The regression: aliased access
;; ---------------------------------------------------------------------------

(deftest missing-inheritance-parent-is-rejected
  (let [create-result (.execute *handler*
                                "CREATE TABLE orphan (note text) INHERITS (missing_parent)")]
    (is (= "42P01" (.-sqlstate ^PgWireServer$QueryResult create-result)))
    (is (= "relation \"missing_parent\" does not exist" (.-error create-result)))
    (is (= "42P01"
           (.-sqlstate ^PgWireServer$QueryResult
            (.execute *handler* "SELECT * FROM orphan"))))))

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

(deftest implicit-insert-column-order-includes-inherited-columns
  ;; PostgreSQL orders inherited columns before the child's own columns when
  ;; INSERT omits its target list. The child's persisted column-order contains
  ;; only `cname`, so translation must prepend the parent's order explicitly.
  (.execute *handler* "INSERT INTO chi VALUES (3, 'r', 30, 'e')")
  (is (= [["3" "r" "30" "e"]]
         (rows "SELECT id, pname, pnum, cname FROM chi WHERE id = 3"))))

(deftest inherited-columns-have-child-specific-catalog-addresses
  (is (= [["id" "1"] ["pname" "2"] ["pnum" "3"] ["cname" "4"]]
         (rows (str "SELECT a.attname, a.attnum FROM pg_attribute a "
                    "JOIN pg_class c ON c.oid = a.attrelid "
                    "WHERE c.relname = 'chi' ORDER BY a.attnum"))))
  (is (= [["chi" "1"] ["par" "1"]]
         (rows (str "SELECT c.relname, a.attnum FROM pg_attribute a "
                    "JOIN pg_class c ON c.oid = a.attrelid "
                    "WHERE a.attname = 'id' AND c.relname IN ('par', 'chi') "
                    "ORDER BY c.relname")))))

(deftest a-parent-with-live-children-cannot-be-dropped
  (let [result (.execute *handler* "DROP TABLE par")]
    (is (= "2BP01" (.-sqlstate ^PgWireServer$QueryResult result)))
    (is (= [["1" "p"] ["2" "q"]]
           (rows "SELECT id, pname FROM chi ORDER BY id")))))

(deftest added-parent-columns-propagate-durable-child-addresses
  ;; PostgreSQL permits omitting the noise word COLUMN; cover that spelling as
  ;; well as propagation through more than one generation.
  (.execute *handler* "CREATE TABLE add_grand(extra text) INHERITS(chi)")
  (is (nil? (.-error ^PgWireServer$QueryResult
             (.execute *handler* "ALTER TABLE par ADD added int"))))
  (.execute *handler*
            "INSERT INTO add_grand(id,pname,pnum,cname,extra,added) VALUES(8,'p',80,'c','g',9)")
  (is (= [["added" "5"]]
         (rows (str "SELECT a.attname,a.attnum FROM pg_attribute a "
                    "JOIN pg_class c ON c.oid=a.attrelid "
                    "WHERE c.relname='chi' AND a.attname='added'"))))
  (is (= [["9"]] (rows "SELECT added FROM add_grand WHERE id=8"))))

(deftest parent-add-does-not-reorder-positional-child-inserts
  (.execute *handler* "CREATE TABLE order_parent(a int)")
  (.execute *handler* "CREATE TABLE order_child(c int) INHERITS(order_parent)")
  (.execute *handler* "ALTER TABLE order_parent ADD b int")
  (is (nil? (.-error ^PgWireServer$QueryResult
             (.execute *handler* "INSERT INTO order_child VALUES(1,2,3)"))))
  (is (= [["1" "2" "3"]]
         (rows "SELECT a,c,b FROM order_child"))))

(deftest dropping-a-child-retires-its-inheritance-edge
  (.execute *handler* "CREATE TABLE drop_parent(a int)")
  (.execute *handler* "CREATE TABLE drop_child(b int) INHERITS(drop_parent)")
  (is (nil? (.-error ^PgWireServer$QueryResult
             (.execute *handler* "DROP TABLE drop_child"))))
  (is (nil? (.-error ^PgWireServer$QueryResult
             (.execute *handler* "DROP TABLE drop_parent"))))
  (.execute *handler* "CREATE TABLE drop_parent(a int)")
  (.execute *handler* "CREATE TABLE drop_child(b int)")
  (is (nil? (.-error ^PgWireServer$QueryResult
             (.execute *handler* "DROP TABLE drop_parent")))))

(deftest transitive-inheritance-preserves-columns-and-markers
  (.execute *handler* "CREATE TABLE grand (gname text) INHERITS (chi)")
  (.execute *handler* "INSERT INTO grand VALUES (4, 's', 40, 'f', 'g')")
  (is (= [["4" "s" "40" "f" "g"]]
         (rows "SELECT id, pname, pnum, cname, gname FROM grand WHERE id = 4")))
  (is (= [["4" "s" "f"]]
         (rows "SELECT id, pname, cname FROM chi WHERE id = 4"))
      "the immediate parent sees descendant rows")
  (is (= [["4" "s"]]
         (rows "SELECT id, pname FROM par WHERE id = 4"))
      "the root parent sees transitive descendant rows"))

(deftest empty-inheritance-chain-after-ctas-accepts-an-implicit-insert
  ;; PostgreSQL rowtypes.sql:463-470. Empty children contribute no columns;
  ;; the implicit INSERT order still walks through them to the CTAS root.
  (.execute *handler* "CREATE TEMP TABLE ct_root AS SELECT id, pnum FROM par")
  ;; Keep the lowercase spelling used by PostgreSQL's rowtypes.sql: SQL
  ;; keywords are case-insensitive, including parser option tokens.
  (.execute *handler* "CREATE TEMP TABLE ct_child () inherits (ct_root)")
  (is (= "INSERT 0 1"
         (.-commandTag ^PgWireServer$QueryResult
          (.execute *handler* "INSERT INTO ct_child VALUES (8, 80)"))))
  (.execute *handler* "CREATE TEMP TABLE ct_grandchild () inherits (ct_child)")
  (is (= "INSERT 0 1"
         (.-commandTag ^PgWireServer$QueryResult
          (.execute *handler* "INSERT INTO ct_grandchild VALUES (9, 90)"))))
  (is (= [["8" "80"] ["9" "90"]]
         (rows "SELECT id, pnum FROM ct_root WHERE id IN (8, 9) ORDER BY id"))))

(deftest the-parent-table-is-unaffected
  (.execute *handler* "INSERT INTO par (id, pname, pnum) VALUES (9, 'z', 90)")
  (is (= "z" (v "SELECT p.pname FROM par p WHERE p.id = 9")))
  (is (= "z" (v "SELECT pname FROM par WHERE id = 9"))))

(deftest physical-inheritance-keeps-keys-and-foreign-actions-table-local
  (doseq [sql ["CREATE TABLE actions(id serial PRIMARY KEY, label text)"
               "CREATE TABLE windows(PRIMARY KEY(id)) INHERITS(actions)"
               "CREATE TABLE action_ref(id int PRIMARY KEY, action_id int REFERENCES actions(id) ON DELETE CASCADE)"
               "CREATE TABLE window_ref(id int PRIMARY KEY, action_id int REFERENCES windows(id) ON DELETE CASCADE)"
               "INSERT INTO actions(id,label) VALUES(7,'parent')"
               "INSERT INTO windows(id,label) VALUES(7,'child')"
               "INSERT INTO windows(label) VALUES('from default')"
               "INSERT INTO action_ref VALUES(1,7)"
               "INSERT INTO window_ref VALUES(1,7)"]]
    (exec! sql))
  (is (= [["windows" "1" "from default"] ["windows" "7" "child"] ["actions" "7" "parent"]]
         (rows "SELECT tableoid::regclass, id, label FROM actions ORDER BY id,label")))
  (is (= [["actions" "7" "parent"]] (rows "SELECT tableoid::regclass,id,label FROM ONLY actions")))
  (is (= "23503" (.-sqlstate ^PgWireServer$QueryResult (execute "INSERT INTO action_ref VALUES(2,1)"))))
  (exec! "BEGIN")
  (is (= #{["7" "changed child"] ["7" "changed parent"]}
         (set (rows "UPDATE actions SET label='changed '||label WHERE id=7 RETURNING id,label"))))
  (exec! "DELETE FROM actions WHERE id=7")
  (is (= [["0" "0"]]
         (rows "SELECT (SELECT count(*) FROM action_ref), (SELECT count(*) FROM window_ref)")))
  (exec! "ROLLBACK")
  (exec! "CREATE TABLE requests(requested int)")
  (exec! "INSERT INTO requests VALUES(7),(19)")
  (is (= [["7" "child"] ["7" "parent"] ["19" nil]]
         (rows "SELECT r.requested,p.label FROM requests r LEFT JOIN actions p ON r.requested=p.id ORDER BY r.requested,p.label")))
  (exec! "DELETE FROM ONLY windows WHERE id=7")
  (is (= [["1" "0"]]
         (rows "SELECT (SELECT count(*) FROM action_ref), (SELECT count(*) FROM window_ref)")))
  (is (= "23503" (.-sqlstate ^PgWireServer$QueryResult (execute "INSERT INTO window_ref VALUES(2,7)"))))
  (is (= [["actions" "7" "parent"] ["windows" "1" "from default"]]
         (rows "SELECT tableoid::regclass,id,label FROM actions ORDER BY label DESC")))
  (exec! "ALTER TABLE windows ALTER COLUMN label SET NOT NULL")
  (is (= "23502" (.-sqlstate (execute "INSERT INTO windows(id) VALUES(20)")))))

(deftest postgres-inheritance-child-key-nullability
  (exec! "CREATE TABLE a (aa text)")
  (exec! "CREATE TEMP TABLE z (b TEXT, PRIMARY KEY(aa, b)) inherits (a)")
  (is (= "23502" (.-sqlstate (execute "INSERT INTO z VALUES (NULL, 'text')"))))
  (exec! "CREATE TEMP TABLE z2 (b TEXT, UNIQUE(aa, b)) inherits (a)")
  (exec! "INSERT INTO z2 VALUES (NULL, 'text')")
  (is (= [[nil "text"]] (rows "SELECT * FROM z2"))))

(deftest postgres-inheritance-cascade-excludes-child-table
  (doseq [sql ["create table inh_fk_1 (a int primary key)"
               "insert into inh_fk_1 values (1), (2), (3)"
               "create table inh_fk_2 (x int primary key, y int references inh_fk_1 on delete cascade)"
               "insert into inh_fk_2 values (11, 1), (22, 2), (33, 3)"
               "create table inh_fk_2_child () inherits (inh_fk_2)"
               "insert into inh_fk_2_child values (111, 1), (222, 2)"
               "delete from inh_fk_1 where a = 1"]]
    (exec! sql))
  (is (= [["2"] ["3"]] (rows "select * from inh_fk_1 order by 1")))
  (is (= [["22" "2"] ["33" "3"] ["111" "1"] ["222" "2"]]
         (rows "select * from inh_fk_2 order by 1, 2"))))

(deftest postgres-inheritance-child-serial-defaults
  (doseq [sql ["create table matest0 (id serial primary key, name text)"
               "create table matest1 (id integer primary key) inherits (matest0)"
               "create table matest2 (id integer primary key) inherits (matest0)"
               "create table matest3 (id integer primary key) inherits (matest0)"
               "insert into matest1 (name) values ('Test 1')"
               "insert into matest1 (name) values ('Test 2')"
               "insert into matest2 (name) values ('Test 3')"
               "insert into matest2 (name) values ('Test 4')"
               "insert into matest3 (name) values ('Test 5')"
               "insert into matest3 (name) values ('Test 6')"]]
    (exec! sql))
  (is (= [["1" "Test 1"] ["2" "Test 2"] ["3" "Test 3"]
          ["4" "Test 4"] ["5" "Test 5"] ["6" "Test 6"]]
         (rows "SELECT * FROM matest0 ORDER BY id")))
  (is (empty? (rows "SELECT * FROM ONLY matest0"))))

(deftest recursive-column-not-null
  ;; PostgreSQL 18.6 alter_table.sql, ordinary parent/child nullability.
  (exec! "CREATE TABLE nn_parent (a int)")
  (exec! "CREATE TABLE nn_child (b text) INHERITS (nn_parent)")
  (exec! "CREATE TABLE nn_leaf () INHERITS (nn_child)")
  (is (= "0A000" (.-sqlstate (execute "ALTER TABLE ONLY nn_parent ALTER COLUMN a SET NOT NULL"))))
  (exec! "INSERT INTO nn_leaf DEFAULT VALUES")
  (let [result (execute "ALTER TABLE nn_parent ALTER COLUMN a SET NOT NULL")]
    (is (= "23502" (.-sqlstate result)))
    (is (re-find #"nn_leaf" (or (.-error result) ""))))
  (exec! "INSERT INTO nn_parent DEFAULT VALUES")
  (exec! "DELETE FROM nn_parent")
  (exec! "ALTER TABLE nn_parent ALTER COLUMN a SET NOT NULL")
  (exec! "ALTER TABLE ONLY nn_parent ALTER COLUMN a SET NOT NULL")
  (doseq [target ["nn_parent" "nn_child" "nn_leaf"]]
    (is (= "23502" (.-sqlstate (execute (str "INSERT INTO " target " DEFAULT VALUES"))))))
  (exec! "CREATE TABLE nn_new () INHERITS (nn_parent)")
  (is (= "23502" (.-sqlstate (execute "INSERT INTO nn_new DEFAULT VALUES")))))

(deftest only-set-not-null-without-descendants
  ;; PostgreSQL 18.6 constraints.sql, ALTER TABLE ONLY on an inherited leaf.
  (exec! "CREATE TABLE nn_parent (a int)")
  (exec! "CREATE TABLE nn_leaf () INHERITS (nn_parent)")
  (exec! "INSERT INTO nn_leaf DEFAULT VALUES")
  (is (= "23502" (.-sqlstate (execute "ALTER TABLE ONLY nn_leaf ALTER COLUMN a SET NOT NULL"))))
  (exec! "DELETE FROM nn_leaf")
  (exec! "ALTER TABLE ONLY nn_leaf ALTER COLUMN a SET NOT NULL")
  (exec! "INSERT INTO nn_parent DEFAULT VALUES")
  (is (= "23502" (.-sqlstate (execute "INSERT INTO nn_leaf DEFAULT VALUES"))))
  (exec! "CREATE TABLE nn_grandchild () INHERITS (nn_leaf)")
  (is (= "23502" (.-sqlstate (execute "INSERT INTO nn_grandchild DEFAULT VALUES"))))
  (exec! "CREATE TABLE nn_standalone (a int)")
  (exec! "ALTER TABLE ONLY nn_standalone ALTER COLUMN a SET NOT NULL")
  (is (= "23502" (.-sqlstate (execute "INSERT INTO nn_standalone DEFAULT VALUES")))))

(deftest new-child-inherits-not-null-from-each-parent
  (exec! "CREATE TABLE nn_optional (a int)")
  (exec! "CREATE TABLE nn_required (a int)")
  (exec! "ALTER TABLE nn_required ALTER COLUMN a SET NOT NULL")
  (exec! "CREATE TABLE nn_child () INHERITS (nn_optional, nn_required)")
  (exec! "CREATE TABLE nn_local (a int) INHERITS (nn_optional, nn_required)")
  (exec! "INSERT INTO nn_optional DEFAULT VALUES")
  (doseq [target ["nn_child" "nn_local"]]
    (is (= "23502" (.-sqlstate (execute (str "INSERT INTO " target " DEFAULT VALUES")))))
    (exec! (str "INSERT INTO " target " (a) VALUES (1)"))))

(deftest postgres-inheritance-recursive-not-null
  ;; PostgreSQL 18.6 inherit.sql lines 829-833, including converging paths.
  (doseq [sql ["create table pp1 (f1 int)"
               "create table cc1 (f2 text, f3 int) inherits (pp1)"
               "create table cc2 (f4 float) inherits (pp1,cc1)"
               "create table cc3 () inherits (pp1,cc1,cc2)"
               "alter table pp1 alter f1 set not null"]]
    (exec! sql))
  (doseq [target ["pp1" "cc1" "cc2" "cc3"]]
    (is (= "23502" (.-sqlstate (execute (str "INSERT INTO " target " (f1) VALUES (NULL)")))))))

(deftest inherited-columns-added-after-child-creation
  ;; Odoo creates its ir_actions descendants before reflecting model fields.
  (exec! "CREATE TABLE actions (id int)")
  (exec! "CREATE TABLE windows () INHERITS (actions)")
  (exec! "CREATE TABLE dialogs () INHERITS (windows)")
  (exec! "ALTER TABLE actions ADD COLUMN label varchar(30)")
  (exec! "INSERT INTO dialogs (id, label) VALUES (1, 'named')")
  (is (= [["1" "named"]] (rows "SELECT id,label FROM actions")))
  (exec! "ALTER TABLE windows ALTER COLUMN label SET NOT NULL")
  (exec! "INSERT INTO actions (id) VALUES (2)")
  (is (= "23502" (.-sqlstate (execute "INSERT INTO dialogs (id) VALUES (3)"))))
  (is (= "23502" (.-sqlstate (execute "ALTER TABLE actions ALTER COLUMN label SET NOT NULL"))))
  (exec! "UPDATE actions SET label='filled' WHERE id=2")
  (exec! "ALTER TABLE actions ALTER COLUMN label SET NOT NULL")
  (is (= "23502" (.-sqlstate (execute "INSERT INTO actions (id) VALUES (4)")))))

(deftest add-column-merges-with-child-local-storage
  (exec! "CREATE TABLE p_merge (id int)")
  (exec! "CREATE TABLE c_merge (note text) INHERITS (p_merge)")
  (exec! "CREATE TABLE g_merge () INHERITS (c_merge)")
  (exec! "INSERT INTO g_merge VALUES (1,'existing')")
  (exec! "ALTER TABLE p_merge ADD COLUMN note text")
  (is (= [["existing"]] (rows "SELECT note FROM p_merge")))
  (exec! "ALTER TABLE p_merge ALTER COLUMN note SET NOT NULL")
  (is (= "23502" (.-sqlstate (execute "INSERT INTO g_merge (id) VALUES (2)")))))
