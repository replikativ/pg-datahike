(ns datahike.test.pg-expression-default-test
  "A column DEFAULT that is an arbitrary scalar expression, not one of a
   fixed handful of spellings.

   The old `column-default-spec` recognised literals, five zero-arg
   functions and `nextval`, and raised `feature-not-supported` for
   everything else -- which failed the whole CREATE TABLE, and with it
   every later statement in the file that referenced the table. PostgreSQL
   evaluates a default like any other expression, against an empty tuple,
   and so do we now: the text is stored, parsed once per statement, and
   evaluated per write by the same translator a CHECK constraint uses.

   Per WRITE is the part worth testing. Folding the expression at CREATE
   TABLE would be cheaper and wrong -- every row would get one frozen
   `gen_random_uuid()`."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.dump :as dump]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)
(def ^:dynamic *conn* nil)

(defn- jdbc-url [port] (str "jdbc:postgresql://localhost:" port "/datahike"
                            "?user=datahike&password=datahike"))

(defn- server-fixture [f]
  (Class/forName "org.postgresql.Driver")
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          srv  (pg/start-server conn {:port 0})
          port (.getPort ^datahike.pg.PgWireServer (:server srv))]
      (try
        (binding [*port* port *conn* conn] (f))
        (finally
          (.stop ^datahike.pg.PgWireServer (:server srv))
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each server-fixture)

(defn- exec! [^Connection c ^String sql]
  (with-open [stmt (.createStatement c)] (.execute stmt sql)))

(defn- rows [^Connection c ^String sql]
  (with-open [stmt (.createStatement c)
              rs (.executeQuery stmt sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [out []]
        (if (.next rs)
          (recur (conj out (mapv #(.getObject rs (int %)) (range 1 (inc n)))))
          out)))))

(deftest arithmetic-and-function-defaults
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c (str "CREATE TABLE t (a int, b int DEFAULT 100 + 1,"
                  " c text DEFAULT upper('ab'), d int DEFAULT abs(-3),"
                  " e text DEFAULT left('abcd', 2))"))
    (exec! c "INSERT INTO t (a) VALUES (1)")
    (is (= [[1 101 "AB" 3 "ab"]] (rows c "SELECT a,b,c,d,e FROM t")))
    (testing "`abs(-3)` and `left(...)` need the DEFAULT paren rewrite"
      ;; A signed argument, and a name the grammar reserves for LEFT JOIN.
      ;; Both are parse errors bare and parse wrapped, which is what the
      ;; rule does -- it used to fire only for nextval/currval/lastval.
      (is (= [[3 "ab"]] (rows c "SELECT d,e FROM t"))))))

(deftest a-volatile-default-is-evaluated-per-row
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TABLE t (a int, u uuid DEFAULT gen_random_uuid())")
    (exec! c "INSERT INTO t (a) VALUES (1), (2), (3)")
    (is (= [[3]] (rows c "SELECT count(DISTINCT u) FROM t"))
        "one frozen uuid for every row means the default was folded")))

(deftest every-path-that-fills-a-default
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TABLE t (a int, b int DEFAULT 100 + 1)")
    (testing "column omitted from the column list"
      (exec! c "INSERT INTO t (a) VALUES (1)")
      (is (= [[1 101]] (rows c "SELECT a,b FROM t WHERE a = 1"))))
    (testing "the DEFAULT keyword in VALUES"
      (exec! c "INSERT INTO t (a, b) VALUES (2, DEFAULT)")
      (is (= [[2 101]] (rows c "SELECT a,b FROM t WHERE a = 2"))))
    (testing "UPDATE ... SET col = DEFAULT"
      (exec! c "INSERT INTO t (a, b) VALUES (3, 7)")
      (exec! c "UPDATE t SET b = DEFAULT WHERE a = 3")
      (is (= [[3 101]] (rows c "SELECT a,b FROM t WHERE a = 3"))))
    (testing "DEFAULT VALUES"
      (exec! c "CREATE TABLE u (a int DEFAULT 7 + 1)")
      (exec! c "INSERT INTO u DEFAULT VALUES")
      (is (= [[8]] (rows c "SELECT a FROM u"))))))

(deftest a-default-that-cannot-be-evaluated-says-so
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (testing "a column reference"
      ;; PostgreSQL rejects this at CREATE TABLE ("cannot use column
      ;; reference in DEFAULT expression"); we reject it at the write,
      ;; because the default is evaluated with no row in scope and the
      ;; name simply does not resolve. Different moment, same refusal --
      ;; what matters is that it is not silently NULL.
      (exec! c "CREATE TABLE t (a int, b int DEFAULT a + 1)")
      (let [e (is (thrown? SQLException (exec! c "INSERT INTO t (a) VALUES (1)")))]
        (is (= "42703" (.getSQLState ^SQLException e)))))
    (testing "an unknown function"
      (exec! c "CREATE TABLE v (a int, b int DEFAULT no_such_fn(1))")
      (let [e (is (thrown? SQLException (exec! c "INSERT INTO v (a) VALUES (1)")))]
        (is (= "42883" (.getSQLState ^SQLException e)))))))

(deftest the-catalog-and-a-dump-show-the-expression
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c (str "CREATE TABLE t (a int DEFAULT 100 + 1,"
                  " b text DEFAULT upper('ab'), d timestamp DEFAULT now())"))
    ;; The parsed form, not the tokens: the column specs arrive tokenized,
    ;; so the raw text reads "100 +1" and "upper ('ab')".
    (is (= [["100 + 1"] ["upper('ab')"] ["now()"]]
           (rows c (str "SELECT pg_get_expr(adbin, adrelid) FROM pg_attrdef"
                        " WHERE adrelid = 't'::regclass ORDER BY adnum"))))
    (testing "a dump writes every default back out"
      ;; `now()` is the regression here. dump.clj matched a `:now` kind
      ;; that ddl.clj never stores -- it stores `:fn` with the name in
      ;; :pg/default-value -- so `DEFAULT now()` fell through to nil and
      ;; was dropped from the dump entirely. A restore silently lost it.
      (let [sql (str/join "\n" (dump/dump *conn* {:sections #{:schema}}))
            create (first (filter #(str/includes? % "CREATE TABLE \"t\"")
                                  (str/split sql #";\n")))]
        (is (some? create))
        (is (str/includes? create "DEFAULT 100 + 1"))
        (is (str/includes? create "DEFAULT upper('ab')"))
        (is (str/includes? create "DEFAULT now()"))))))

;; ---------------------------------------------------------------------------
;; ALTER TABLE ADD CONSTRAINT
;;
;; `sql.clj` mapped every ADD that was not PRIMARY KEY or UNIQUE onto
;; `{:op :add-constraint}`, and nothing consumed it. The statement replied
;; ALTER TABLE and added no constraint: a CHECK that should have refused a
;; row let it through, and a FOREIGN KEY never looked for its parent.
;; ---------------------------------------------------------------------------

(deftest alter-add-check-constrains
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TABLE t (a int, b int)")
    (exec! c "INSERT INTO t VALUES (1, 1)")
    (exec! c "ALTER TABLE t ADD CONSTRAINT tc CHECK (a > 0)")
    (let [e (is (thrown? SQLException (exec! c "INSERT INTO t VALUES (-5, 1)")))]
      (is (= "23514" (.getSQLState ^SQLException e))))
    (is (= [[1]] (rows c "SELECT count(*) FROM t")))
    (testing "the unnamed spelling, and PostgreSQL's generated names"
      ;; `ADD CHECK (...)` without CONSTRAINT is a JSqlParser parse error;
      ;; `alter-add-check-rule` names it so the parser accepts it, and the
      ;; executor then generates the name PostgreSQL would.
      (exec! c "ALTER TABLE t ADD CHECK (b > 0)")
      (exec! c "ALTER TABLE t ADD CHECK (b < 100)")
      ;; `t_b_check`, not `t_check`: ChooseConstraintName uses
      ;; `<table>_<col>_check` when the expression references exactly one
      ;; column, and the ALTER path used to pass nil for the column -- so
      ;; CREATE TABLE and ALTER named the same constraint differently.
      ;; A 17.7 oracle gives tc, tchk_b_check, tchk_b_check1 for this
      ;; sequence.
      (is (= [["t_b_check"] ["t_b_check1"] ["tc"]]
             (rows c (str "SELECT conname FROM pg_constraint"
                          " WHERE conrelid = 't'::regclass AND contype = 'c'"
                          " ORDER BY conname"))))
      (is (thrown? SQLException (exec! c "INSERT INTO t VALUES (1, 500)"))))))

(deftest alter-add-foreign-key-constrains
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TABLE p (b int PRIMARY KEY)")
    (exec! c "CREATE TABLE ch (a int)")
    (exec! c "INSERT INTO p VALUES (1)")
    (exec! c "ALTER TABLE ch ADD CONSTRAINT chfk FOREIGN KEY (a) REFERENCES p(b)")
    (exec! c "INSERT INTO ch VALUES (1)")
    (let [e (is (thrown? SQLException (exec! c "INSERT INTO ch VALUES (9)")))]
      (is (= "23503" (.getSQLState ^SQLException e))))
    (testing "a NULL key is satisfied vacuously, as MATCH SIMPLE says"
      (exec! c "INSERT INTO ch VALUES (NULL)")
      (is (= [[2]] (rows c "SELECT count(*) FROM ch"))))
    (testing "two unnamed keys get distinct generated names"
      ;; The generated-name set was seeded from CHECK constraints only, so
      ;; a second unnamed FK produced the name the first already had and
      ;; the two entities collided on :pg/constraint-key -- the second
      ;; silently replaced the first.
      (exec! c "CREATE TABLE ch2 (a int)")
      (exec! c "ALTER TABLE ch2 ADD FOREIGN KEY (a) REFERENCES p")
      (exec! c "ALTER TABLE ch2 ADD FOREIGN KEY (a) REFERENCES p(b) MATCH FULL")
      (is (= [["ch2_a_fkey"] ["ch2_a_fkey1"]]
             (rows c (str "SELECT conname FROM pg_constraint"
                          " WHERE conrelid = 'ch2'::regclass AND contype = 'f'"
                          " ORDER BY conname")))))))

(deftest alter-add-constraint-validates-the-rows-already-there
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (testing "CHECK"
      (exec! c "CREATE TABLE t (a int)")
      (exec! c "INSERT INTO t VALUES (-5), (3)")
      (let [e (is (thrown? SQLException
                           (exec! c "ALTER TABLE t ADD CONSTRAINT tc CHECK (a > 0)")))]
        (is (= "23514" (.getSQLState ^SQLException e))))
      ;; Refused means not added: the violating row is still insertable.
      (exec! c "INSERT INTO t VALUES (-6)")
      (is (= [[3]] (rows c "SELECT count(*) FROM t"))))
    (testing "FOREIGN KEY"
      (exec! c "CREATE TABLE p (b int PRIMARY KEY)")
      (exec! c "CREATE TABLE ch (a int)")
      (exec! c "INSERT INTO p VALUES (1)")
      (exec! c "INSERT INTO ch VALUES (1), (99)")
      (let [e (is (thrown? SQLException
                           (exec! c (str "ALTER TABLE ch ADD CONSTRAINT f"
                                         " FOREIGN KEY (a) REFERENCES p(b)"))))]
        (is (= "23503" (.getSQLState ^SQLException e)))))))

(deftest fk-match-type
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TABLE p (a int PRIMARY KEY)")
    (testing "inline, where the rule used to strand the MATCH clause"
      ;; `inline-references-rule` lifted the REFERENCES to a table-level FK
      ;; and left `MATCH FULL` behind in the column definition.
      (exec! c "CREATE TABLE ch (a int REFERENCES p(a) MATCH FULL)")
      (exec! c "INSERT INTO p VALUES (1)")
      (exec! c "INSERT INTO ch VALUES (1)")
      (is (thrown? SQLException (exec! c "INSERT INTO ch VALUES (2)"))
          "the lifted FK must still be enforced"))
    (testing "table-level, which JSqlParser cannot parse at all"
      (exec! c "CREATE TABLE ch2 (a int, FOREIGN KEY (a) REFERENCES p(a) MATCH FULL)")
      (is (thrown? SQLException (exec! c "INSERT INTO ch2 VALUES (7)"))))
    (testing "a multi-column MATCH FULL is refused, not quietly downgraded"
      ;; MATCH FULL and MATCH SIMPLE differ only when some key columns are
      ;; NULL and others are not -- impossible for one column, real for
      ;; two. Accepting the syntax and enforcing MATCH SIMPLE underneath
      ;; would be a silently wrong answer.
      (exec! c "CREATE TABLE q (x int, y int, PRIMARY KEY (x, y))")
      (let [e (is (thrown? SQLException
                           (exec! c (str "CREATE TABLE ch3 (a int, b int,"
                                         " FOREIGN KEY (a, b) REFERENCES q(x, y)"
                                         " MATCH FULL)"))))]
        (is (= "0A000" (.getSQLState ^SQLException e))))
      (testing "MATCH SIMPLE on the same key is the default, so it is fine"
        (exec! c (str "CREATE TABLE ch4 (a int, b int,"
                      " FOREIGN KEY (a, b) REFERENCES q(x, y) MATCH SIMPLE)"))))))
