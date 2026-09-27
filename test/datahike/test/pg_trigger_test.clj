(ns datahike.test.pg-trigger-test
  "BEFORE ROW triggers on INSERT.

   A trigger is not a function call in a query: it is fired by the WRITE
   path, per row. The hook is `datahike.pg.constraints.row`'s per-row
   sequence, and the placement follows PostgreSQL's:
   `ExecBRInsertTriggers` runs AHEAD of `ExecConstraints`
   (nodeModifyTable.c 935 / 1118), so a BEFORE trigger may fix up a row
   that would otherwise fail a constraint.

   What a BEFORE ROW trigger returns IS the row that gets written.
   `RETURN NEW` writes it (with whatever the body assigned to its
   fields), `RETURN NULL` suppresses it entirely, and a suppressed row
   is not counted by the CommandComplete tag either.

   Triggers are gated on plpgsql by construction: PostgreSQL refuses a
   `LANGUAGE sql` trigger function outright (\"SQL functions cannot
   return type trigger\"), and 222 of the 230 trigger bodies in its own
   regression corpus are plpgsql.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.sql.classify :as c]
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

(defn- err [^PgWireServer$QueryResult r] (some-> (.error r) str))

(defn- ok! [h sql]
  (let [r (exec h sql)]
    (is (nil? (.error r)) (str sql " => " (err r)))
    r))

(defn- release! [{:keys [conn cfg]}] (d/release conn) (d/delete-database cfg))

(defmacro with-h [[sym init] & body]
  `(let [~sym ~init] (try ~@body (finally (release! ~sym)))))

(def ^:private dq "$$")

(defn- trigfn! [h name body]
  (ok! h (str "CREATE FUNCTION " name "() RETURNS trigger AS " dq body dq
              " LANGUAGE plpgsql")))

;; ============================================================================
;; The statement
;; ============================================================================

(deftest create-trigger-parses
  (testing "timing, the event list, the level and the function"
    (let [r (c/classify "CREATE TRIGGER t BEFORE INSERT ON m FOR EACH ROW EXECUTE PROCEDURE f()")]
      (is (= :create-trigger-plpgsql (:kind r)))
      (is (= [:before [:insert] :row "f"] [(:timing r) (:events r) (:level r) (:function r)])))
    (let [r (c/classify "CREATE TRIGGER t AFTER INSERT OR UPDATE OR DELETE ON m FOR EACH STATEMENT EXECUTE FUNCTION g('a','b')")]
      (is (= [:after [:insert :update :delete] :statement ["a" "b"]]
             [(:timing r) (:events r) (:level r) (:arguments r)]))))
  (testing "FOR EACH STATEMENT is the default, as in PostgreSQL"
    (is (= :statement (:level (c/classify "CREATE TRIGGER t AFTER UPDATE ON m EXECUTE PROCEDURE k()")))))
  (testing "UPDATE OF carries its column list"
    (is (= #{"a" "b"}
           (:update-columns
            (c/classify "CREATE TRIGGER t BEFORE UPDATE OF a, b ON m FOR EACH ROW EXECUTE PROCEDURE h()")))))
  (testing "WHEN keeps the condition's SOURCE — joining tokens loses the spelling"
    (is (= "NEW.i > 5"
           (:when-condition
            (c/classify "CREATE TRIGGER t BEFORE INSERT ON m FOR EACH ROW WHEN (NEW.i > 5) EXECUTE PROCEDURE f()")))))
  (testing "a constraint trigger keeps its reject — it is deferrable, which the firing path does not model"
    (is (= :create-trigger
           (:kind (c/classify "CREATE CONSTRAINT TRIGGER t AFTER INSERT ON m DEFERRABLE FOR EACH ROW EXECUTE PROCEDURE f()")))))
  (testing "DROP TRIGGER … ON table"
    (is (= ["t" "m" false] (let [r (c/classify "DROP TRIGGER t ON m")]
                             [(:trigger-name r) (:table r) (:if-exists? r)])))
    (is (true? (:if-exists? (c/classify "DROP TRIGGER IF EXISTS t ON m"))))))

(deftest ddl-errors-match-postgresql
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int)")
    (trigfn! h "f" "BEGIN RETURN NEW; END")
    (ok! h "CREATE TRIGGER a BEFORE INSERT ON t FOR EACH ROW EXECUTE PROCEDURE f()")
    (is (str/includes? (err (exec h "CREATE TRIGGER a BEFORE INSERT ON t FOR EACH ROW EXECUTE PROCEDURE f()"))
                       "trigger \"a\" for relation \"t\" already exists"))
    (is (str/includes? (err (exec h "CREATE TRIGGER b BEFORE INSERT ON nosuch FOR EACH ROW EXECUTE PROCEDURE f()"))
                       "relation \"nosuch\" does not exist"))
    (is (str/includes? (err (exec h "CREATE TRIGGER c BEFORE INSERT ON t FOR EACH ROW EXECUTE PROCEDURE nofn()"))
                       "function nofn() does not exist"))
    (testing "DROP, and its 42704 and its notice"
      (ok! h "DROP TRIGGER a ON t")
      (is (str/includes? (err (exec h "DROP TRIGGER a ON t"))
                         "trigger \"a\" for table \"t\" does not exist"))
      (ok! h "DROP TRIGGER IF EXISTS a ON t"))))

;; ============================================================================
;; Firing
;; ============================================================================

(deftest a-before-row-trigger-rewrites-the-row
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int, s text)")
    (trigfn! h "stamp" "BEGIN NEW.s := 'stamped'; RETURN NEW; END")
    (ok! h "CREATE TRIGGER tr BEFORE INSERT ON t FOR EACH ROW EXECUTE PROCEDURE stamp()")
    (ok! h "INSERT INTO t VALUES (1,'orig'),(2,'orig')")
    (is (= [["1" "stamped"] ["2" "stamped"]] (rows (ok! h "SELECT i, s FROM t ORDER BY i"))))))

(deftest returning-null-suppresses-the-row
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int)")
    (trigfn! h "odd" "BEGIN IF NEW.i % 2 = 1 THEN RETURN NULL; END IF; RETURN NEW; END")
    (ok! h "CREATE TRIGGER tr BEFORE INSERT ON t FOR EACH ROW EXECUTE PROCEDURE odd()")
    (let [r (ok! h "INSERT INTO t VALUES (1),(2),(3),(4)")]
      ;; PostgreSQL counts what was written, not what was offered.
      (is (= "INSERT 0 2" (.commandTag r))))
    (is (= [["2"] ["4"]] (rows (ok! h "SELECT i FROM t ORDER BY i"))))))

(deftest several-triggers-run-in-name-order
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int, s text)")
    (trigfn! h "f_a" "BEGIN NEW.s := coalesce(NEW.s,'') || 'a'; RETURN NEW; END")
    (trigfn! h "f_b" "BEGIN NEW.s := coalesce(NEW.s,'') || 'b'; RETURN NEW; END")
    (ok! h "CREATE TRIGGER t2_b BEFORE INSERT ON t FOR EACH ROW EXECUTE PROCEDURE f_b()")
    (ok! h "CREATE TRIGGER t1_a BEFORE INSERT ON t FOR EACH ROW EXECUTE PROCEDURE f_a()")
    (ok! h "INSERT INTO t VALUES (1,'')")
    ;; t1_a before t2_b, whatever order they were created in.
    (is (= [["ab"]] (rows (ok! h "SELECT s FROM t"))))))

(deftest the-tg-variables
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int, s text)")
    (trigfn! h "spy"
             (str "BEGIN NEW.s := TG_NAME || '/' || TG_WHEN || '/' || TG_LEVEL"
                  " || '/' || TG_OP || '/' || TG_TABLE_NAME; RETURN NEW; END"))
    (ok! h "CREATE TRIGGER watcher BEFORE INSERT ON t FOR EACH ROW EXECUTE PROCEDURE spy()")
    (ok! h "INSERT INTO t VALUES (1,'')")
    (is (= [["watcher/BEFORE/ROW/INSERT/t"]] (rows (ok! h "SELECT s FROM t"))))))

(deftest a-trigger-body-can-read-the-database
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int, n int)")
    (ok! h "CREATE TABLE lim (maxv int)")
    (ok! h "INSERT INTO lim VALUES (10)")
    (trigfn! h "clamp"
             (str "DECLARE m int; BEGIN SELECT maxv INTO m FROM lim;"
                  " IF NEW.n > m THEN NEW.n := m; END IF; RETURN NEW; END"))
    (ok! h "CREATE TRIGGER tr BEFORE INSERT ON t FOR EACH ROW EXECUTE PROCEDURE clamp()")
    (ok! h "INSERT INTO t VALUES (1,5),(2,99)")
    (is (= [["1" "5"] ["2" "10"]] (rows (ok! h "SELECT i, n FROM t ORDER BY i"))))))

(deftest a-trigger-that-raises-aborts-the-statement
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int)")
    (trigfn! h "guard" "BEGIN IF NEW.i < 0 THEN RAISE EXCEPTION 'negative: %', NEW.i; END IF; RETURN NEW; END")
    (ok! h "CREATE TRIGGER tr BEFORE INSERT ON t FOR EACH ROW EXECUTE PROCEDURE guard()")
    (ok! h "INSERT INTO t VALUES (1)")
    (let [r (exec h "INSERT INTO t VALUES (-5)")]
      (is (some? (.error r)))
      (is (str/includes? (err r) "negative: -5")))
    ;; and nothing from the failed statement was written
    (is (= [["1"]] (rows (ok! h "SELECT i FROM t ORDER BY i"))))))

(deftest a-dropped-trigger-stops-firing
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int, s text)")
    (trigfn! h "stamp" "BEGIN NEW.s := 'x'; RETURN NEW; END")
    (ok! h "CREATE TRIGGER tr BEFORE INSERT ON t FOR EACH ROW EXECUTE PROCEDURE stamp()")
    (ok! h "INSERT INTO t VALUES (1,'orig')")
    (ok! h "DROP TRIGGER tr ON t")
    (ok! h "INSERT INTO t VALUES (2,'orig')")
    (is (= [["1" "x"] ["2" "orig"]] (rows (ok! h "SELECT i, s FROM t ORDER BY i"))))))
