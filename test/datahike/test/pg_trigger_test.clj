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
  ;; The row-lock registry is global and a handler used WITHOUT the wire
  ;; layer never ends its implicit transactions -- nothing calls
  ;; `commitImplicit`, so every write leaves its locks behind. They
  ;; accumulate across tests in one JVM until an unrelated statement
  ;; conflicts with a lock some earlier test orphaned, and the failure
  ;; lands wherever the count happened to cross: a plain `DELETE FROM
  ;; log` reporting `deadlock detected: row lock wait timeout`. The
  ;; other fixtures that drive a handler directly already reset it.
  (pg/reset-lock-registry!)
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

(deftest a-trigger-may-assign-to-a-column-the-insert-omitted
  ;; NEW is the whole tuple in PostgreSQL. It was built from the
  ;; candidate entity's own keys here, so a column the INSERT did not
  ;; mention was not a variable at all and the body died with
  ;; `new.s is not a known variable` -- failing the statement. The test
  ;; above did not catch it because its INSERT supplies every column.
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int, s text)")
    (trigfn! h "stamp" "BEGIN NEW.s := 'stamped'; RETURN NEW; END")
    (ok! h "CREATE TRIGGER tr BEFORE INSERT ON t FOR EACH ROW EXECUTE PROCEDURE stamp()")
    (ok! h "INSERT INTO t (i) VALUES (1)")
    (is (= [["1" "stamped"]] (rows (ok! h "SELECT i, s FROM t"))))
    (testing "and may set one back to NULL"
      (trigfn! h "blank" "BEGIN NEW.s := NULL; RETURN NEW; END")
      (ok! h "CREATE TABLE u (i int, s text)")
      (ok! h "CREATE TRIGGER ur BEFORE INSERT ON u FOR EACH ROW EXECUTE PROCEDURE blank()")
      (ok! h "INSERT INTO u VALUES (1, 'given')")
      (is (= [["1" nil]] (rows (ok! h "SELECT i, s FROM u")))))))

(deftest a-before-trigger-runs-ahead-of-the-constraints
  ;; `ExecBRInsertTriggers` before `ExecConstraints` (nodeModifyTable.c),
  ;; which is what lets a trigger fix up a row that would otherwise
  ;; fail. NOT NULL was being checked while the candidate was built --
  ;; before any trigger ran -- so the fixup shape never got the chance.
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int, s text NOT NULL, n int DEFAULT 9, d text)")
    (trigfn! h "fixup"
             (str "BEGIN IF NEW.s IS NULL THEN NEW.s := 'filled'; END IF;"
                  " NEW.d := 'n-was-' || NEW.n::text; RETURN NEW; END"))
    (ok! h "CREATE TRIGGER tr BEFORE INSERT ON t FOR EACH ROW EXECUTE PROCEDURE fixup()")
    (ok! h "INSERT INTO t (i) VALUES (1)")
    (is (= [["1" "filled" "9" "n-was-9"]]
           (rows (ok! h "SELECT i, s, n, d FROM t")))
        "the trigger also sees the DEFAULT already materialised")
    (testing "the constraint still bites when the trigger does not fix it"
      (ok! h "CREATE TABLE u (i int, s text NOT NULL)")
      (trigfn! h "passthru" "BEGIN RETURN NEW; END")
      (ok! h "CREATE TRIGGER ur BEFORE INSERT ON u FOR EACH ROW EXECUTE PROCEDURE passthru()")
      (is (str/includes? (str (err (exec h "INSERT INTO u (i) VALUES (1)")))
                         "not-null")))))

(deftest triggers-chain-in-the-columns-own-types
  ;; plpgsql computes over what a statement produced, which is TEXT, so
  ;; `NEW.f1 := NEW.f1 * 10` hands back a string. The INSERT path
  ;; round-trips the row through `columns->row-entity` between triggers
  ;; and coerces there; the UPDATE path passed the raw map on, so the
  ;; SECOND trigger did arithmetic on a string and the statement died
  ;; with a bare `class java.lang.String cannot be cast to class
  ;; java.lang.Number` -- no SQLSTATE. `triggers.sql` opens with exactly
  ;; this pair.
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (f1 int, f2 text)")
    (trigfn! h "times10" "BEGIN NEW.f1 := NEW.f1 * 10; RETURN NEW; END")
    (ok! h "CREATE TRIGGER alpha BEFORE INSERT OR UPDATE ON t FOR EACH ROW EXECUTE PROCEDURE times10()")
    (ok! h "CREATE TRIGGER zed BEFORE INSERT OR UPDATE ON t FOR EACH ROW EXECUTE PROCEDURE times10()")
    (ok! h "INSERT INTO t VALUES (1, 'foo')")
    (is (= [["100" "foo"]] (rows (ok! h "SELECT f1, f2 FROM t")))
        "both triggers ran on INSERT")
    (ok! h "UPDATE t SET f2 = f2 || 'bar'")
    (is (= [["10000" "foobar"]] (rows (ok! h "SELECT f1, f2 FROM t")))
        "and both on UPDATE, each seeing an int rather than the other's text")))

(deftest returning-null-suppresses-the-row
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int)")
    (trigfn! h "odd" "BEGIN IF NEW.i % 2 = 1 THEN RETURN NULL; END IF; RETURN NEW; END")
    (ok! h "CREATE TRIGGER tr BEFORE INSERT ON t FOR EACH ROW EXECUTE PROCEDURE odd()")
    (let [r (ok! h "INSERT INTO t VALUES (1),(2),(3),(4)")]
      ;; PostgreSQL counts what was written, not what was offered.
      (is (= "INSERT 0 2" (.commandTag r))))
    (is (= [["2"] ["4"]] (rows (ok! h "SELECT i FROM t ORDER BY i"))))))

(deftest dropping-a-table-drops-its-triggers
  ;; A trigger is a dependent of its table, as PostgreSQL makes it. Left
  ;; in the registry, the name was still taken -- a later CREATE TABLE of
  ;; the same name could not define a trigger of the same name -- and the
  ;; orphan matched the new table on every write. Regression files drop
  ;; and recreate tables constantly.
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int)")
    (trigfn! h "noop" "BEGIN RETURN NULL; END")
    (ok! h "CREATE TRIGGER tr AFTER INSERT ON t FOR EACH STATEMENT EXECUTE PROCEDURE noop()")
    (ok! h "DROP TABLE t")
    (ok! h "CREATE TABLE t (i int)")
    (ok! h "CREATE TRIGGER tr AFTER INSERT ON t FOR EACH STATEMENT EXECUTE PROCEDURE noop()")))

(deftest pg-catalog-reports-the-triggers
  ;; `pg_trigger` was a real relation with PostgreSQL 17's columns and no
  ;; rows -- every query against it came back empty, including the ones
  ;; the regression suite uses to check a trigger was created at all.
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int)")
    (ok! h "CREATE TABLE plain (i int)")
    (trigfn! h "noop" "BEGIN RETURN NULL; END")
    (ok! h (str "CREATE TRIGGER b_row BEFORE INSERT OR UPDATE ON t"
                " FOR EACH ROW EXECUTE PROCEDURE noop()"))
    (ok! h (str "CREATE TRIGGER a_stmt AFTER DELETE ON t"
                " FOR EACH STATEMENT EXECUTE PROCEDURE noop()"))
    ;; tgtype is PostgreSQL's bitmask (trigger.h): ROW 1, BEFORE 2,
    ;; INSERT 4, DELETE 8, UPDATE 16. AFTER is the absence of BEFORE.
    (is (= [["a_stmt" "8"] ["b_row" "23"]]
           (rows (ok! h (str "SELECT tgname, tgtype::text FROM pg_trigger"
                             " ORDER BY tgname")))))
    (testing "and joins pg_class on tgrelid"
      (is (= [["2"]]
             (rows (ok! h (str "SELECT count(*)::text FROM pg_trigger tg"
                               " JOIN pg_class c ON c.oid = tg.tgrelid"
                               " WHERE c.relname = 't'"))))))
    (testing "pg_tables.hastriggers is no longer hard-coded false"
      (is (= [["plain" "false"] ["t" "true"]]
             (rows (ok! h (str "SELECT tablename, hastriggers::text FROM pg_tables"
                               " WHERE tablename IN ('t','plain') ORDER BY tablename"))))))))

(deftest transition-tables
  ;; `REFERENCING { OLD | NEW } TABLE AS name` -- the statement's before
  ;; and after images, as relations the body can query. PostgreSQL keeps
  ;; them as tuplestores in the query environment; there is no analogue
  ;; here, so they are materialised as temp tables for the duration of
  ;; the body, created from the source table's own shape.
  ;;
  ;; The clause was PARSED but always rejected -- and it was read after
  ;; `FOR EACH`, where PostgreSQL's grammar puts it before, so it never
  ;; matched anything anyway. 55 of `triggers.sql`'s CREATE TRIGGER
  ;; statements use it.
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (a int, b text)")
    (trigfn! h "report"
             (str "DECLARE n int; s text; BEGIN"
                  " SELECT count(*), string_agg(b, ',' ORDER BY a) INTO n, s"
                  " FROM nt; INSERT INTO log VALUES (n, s); RETURN NULL; END"))
    (ok! h "CREATE TABLE log (n int, s text)")
    (ok! h (str "CREATE TRIGGER tr AFTER INSERT ON t REFERENCING NEW TABLE AS nt"
                " FOR EACH STATEMENT EXECUTE FUNCTION report()"))
    (ok! h "INSERT INTO t VALUES (1,'x'),(2,'y'),(3,'z')")
    (is (= [["3" "x,y,z"]] (rows (ok! h "SELECT n, s FROM log")))
        "the whole statement's rows, in one relation")
    (testing "an empty statement still fires, with an empty relation"
      (ok! h "DELETE FROM log")
      (ok! h "INSERT INTO t SELECT 9, 'q' WHERE false")
      (is (= [["0" nil]] (rows (ok! h "SELECT n, s FROM log")))))
    ;; Last: a failing statement aborts the handler's transaction, so
    ;; nothing may follow it here.
    (testing "the relation does not outlive the body"
      (is (some? (err (exec h "SELECT * FROM nt")))))))

(deftest transition-tables-on-update-and-delete
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (a int, b text)")
    (ok! h "CREATE TABLE log (o text, n text)")
    (trigfn! h "both"
             (str "DECLARE o text; n text; BEGIN"
                  " SELECT string_agg(b, ',' ORDER BY a) INTO o FROM ot;"
                  " SELECT string_agg(b, ',' ORDER BY a) INTO n FROM nt;"
                  " INSERT INTO log VALUES (o, n); RETURN NULL; END"))
    (trigfn! h "gone"
             (str "DECLARE o text; BEGIN"
                  " SELECT string_agg(b, ',' ORDER BY a) INTO o FROM ot;"
                  " INSERT INTO log VALUES (o, NULL); RETURN NULL; END"))
    (ok! h (str "CREATE TRIGGER tu AFTER UPDATE ON t"
                " REFERENCING OLD TABLE AS ot NEW TABLE AS nt"
                " FOR EACH STATEMENT EXECUTE FUNCTION both()"))
    (ok! h (str "CREATE TRIGGER td AFTER DELETE ON t REFERENCING OLD TABLE AS ot"
                " FOR EACH STATEMENT EXECUTE FUNCTION gone()"))
    (ok! h "INSERT INTO t VALUES (1,'x'),(2,'y')")
    (ok! h "UPDATE t SET b = b || '!'")
    (is (= [["x,y" "x!,y!"]] (rows (ok! h "SELECT o, n FROM log"))))
    (ok! h "DELETE FROM log")
    (ok! h "DELETE FROM t WHERE a = 2")
    (is (= [["y!" nil]] (rows (ok! h "SELECT o, n FROM log"))))))

(deftest transition-table-rules-match-postgresql
  ;; trigger.c's own checks, with its own wording.
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (a int, b text)")
    (trigfn! h "noop" "BEGIN RETURN NULL; END")
    (let [bad (fn [sql] (str (err (exec h sql))))]
      (is (str/includes?
           (bad (str "CREATE TRIGGER x BEFORE INSERT ON t REFERENCING NEW TABLE AS nt"
                     " FOR EACH STATEMENT EXECUTE FUNCTION noop()"))
           "transition table name can only be specified for an AFTER trigger"))
      (is (str/includes?
           (bad (str "CREATE TRIGGER x AFTER INSERT OR UPDATE ON t REFERENCING NEW TABLE AS nt"
                     " FOR EACH STATEMENT EXECUTE FUNCTION noop()"))
           "more than one event"))
      (is (str/includes?
           (bad (str "CREATE TRIGGER x AFTER UPDATE OF b ON t REFERENCING NEW TABLE AS nt"
                     " FOR EACH STATEMENT EXECUTE FUNCTION noop()"))
           "column lists"))
      (is (str/includes?
           (bad (str "CREATE TRIGGER x AFTER DELETE ON t REFERENCING NEW TABLE AS nt"
                     " FOR EACH STATEMENT EXECUTE FUNCTION noop()"))
           "NEW TABLE can only be specified for an INSERT or UPDATE trigger"))
      (is (str/includes?
           (bad (str "CREATE TRIGGER x AFTER INSERT ON t REFERENCING OLD TABLE AS ot"
                     " FOR EACH STATEMENT EXECUTE FUNCTION noop()"))
           "OLD TABLE can only be specified for a DELETE or UPDATE trigger")))))

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

;; ============================================================================
;; The rest of the matrix
;; ============================================================================

(deftest statement-level-triggers
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int)")
    (ok! h "CREATE TABLE log (m text)")
    (trigfn! h "note" "BEGIN INSERT INTO log VALUES (TG_NAME || ':' || TG_OP || ':' || TG_LEVEL); RETURN NULL; END")
    (ok! h "CREATE TRIGGER s_ins AFTER INSERT ON t FOR EACH STATEMENT EXECUTE PROCEDURE note()")
    (testing "once per statement, not once per row"
      (ok! h "INSERT INTO t VALUES (1),(2),(3)")
      (is (= [["s_ins:INSERT:STATEMENT"]] (rows (ok! h "SELECT m FROM log")))))
    (testing "and it fires even when the statement matched no rows"
      (ok! h "CREATE TRIGGER s_del AFTER DELETE ON t FOR EACH STATEMENT EXECUTE PROCEDURE note()")
      (ok! h "DELETE FROM t WHERE i = 999")
      (is (= [["s_del:DELETE:STATEMENT"]]
             (rows (ok! h "SELECT m FROM log WHERE m LIKE 's_del%'")))))))

(deftest after-row-triggers
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int)")
    (ok! h "CREATE TABLE log (m text)")
    ;; OLD is not defined for an INSERT trigger, so the body only reads
    ;; NEW -- PostgreSQL raises on a reference to the other one too.
    (trigfn! h "note" "BEGIN INSERT INTO log VALUES (TG_OP || ':' || NEW.i); RETURN NULL; END")
    (ok! h "CREATE TRIGGER r_ins AFTER INSERT ON t FOR EACH ROW EXECUTE PROCEDURE note()")
    (ok! h "INSERT INTO t VALUES (1),(2)")
    ;; Once per row -- and the AFTER trigger's own return value is
    ;; ignored, so RETURN NULL does not suppress anything.
    (is (= [["INSERT:1"] ["INSERT:2"]] (rows (ok! h "SELECT m FROM log ORDER BY m"))))
    (is (= [["1"] ["2"]] (rows (ok! h "SELECT i FROM t ORDER BY i"))))))

(deftest before-update-sees-old-and-new
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int, s text)")
    (ok! h "INSERT INTO t VALUES (1,'a'),(2,'b')")
    (trigfn! h "mark" "BEGIN NEW.s := OLD.s || '>' || NEW.s; RETURN NEW; END")
    (ok! h "CREATE TRIGGER tr BEFORE UPDATE ON t FOR EACH ROW EXECUTE PROCEDURE mark()")
    (ok! h "UPDATE t SET s = 'z' WHERE i = 1")
    (is (= [["1" "a>z"] ["2" "b"]] (rows (ok! h "SELECT i, s FROM t ORDER BY i"))))
    (testing "and can cancel the update for a row"
      (trigfn! h "veto" "BEGIN IF NEW.i = 2 THEN RETURN NULL; END IF; RETURN NEW; END")
      (ok! h "CREATE TRIGGER tr0 BEFORE UPDATE ON t FOR EACH ROW EXECUTE PROCEDURE veto()")
      (let [r (ok! h "UPDATE t SET s = 'q' WHERE i = 2")]
        (is (= "UPDATE 0" (.commandTag r))))
      (is (= [["2" "b"]] (rows (ok! h "SELECT i, s FROM t WHERE i = 2")))))))

(deftest before-delete-can-veto
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int)")
    (ok! h "INSERT INTO t VALUES (1),(2),(3)")
    (trigfn! h "keep2" "BEGIN IF OLD.i = 2 THEN RETURN NULL; END IF; RETURN OLD; END")
    (ok! h "CREATE TRIGGER tr BEFORE DELETE ON t FOR EACH ROW EXECUTE PROCEDURE keep2()")
    (let [r (ok! h "DELETE FROM t")]
      (is (= "DELETE 2" (.commandTag r))))
    (is (= [["2"]] (rows (ok! h "SELECT i FROM t"))))))

(deftest a-when-condition-gates-the-firing
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int, s text)")
    (trigfn! h "stamp" "BEGIN NEW.s := 'big'; RETURN NEW; END")
    (ok! h "CREATE TRIGGER tr BEFORE INSERT ON t FOR EACH ROW WHEN (NEW.i > 5) EXECUTE PROCEDURE stamp()")
    (ok! h "INSERT INTO t VALUES (1,'small'),(9,'x')")
    ;; The body never runs for the row the condition excluded.
    (is (= [["1" "small"] ["9" "big"]] (rows (ok! h "SELECT i, s FROM t ORDER BY i"))))))

(deftest update-of-columns-only-fires-for-those-columns
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (a int, b int, n int)")
    (ok! h "INSERT INTO t VALUES (1,1,0)")
    (trigfn! h "bump" "BEGIN NEW.n := NEW.n + 1; RETURN NEW; END")
    (ok! h "CREATE TRIGGER tr BEFORE UPDATE OF a ON t FOR EACH ROW EXECUTE PROCEDURE bump()")
    (ok! h "UPDATE t SET b = 2")
    (is (= [["0"]] (rows (ok! h "SELECT n FROM t"))) "b is not in the OF list")
    (ok! h "UPDATE t SET a = 2")
    (is (= [["1"]] (rows (ok! h "SELECT n FROM t"))) "a is")))

(deftest a-trigger-row-is-coerced-and-the-verdict-is-kept
  ;; The writeback coerced each column to its declared type and then
  ;; discarded the answer: `(catch Exception _ v)` around the call, so
  ;; every SQLSTATE the coercion exists to raise -- 22P02 bad syntax,
  ;; 22003 out of range, 22001 too long, 22008 an impossible date --
  ;; was swallowed and the UNCOERCED value stored.
  ;;
  ;; It was only half-visible: for a type Datahike carries as a long,
  ;; its own schema check caught the value afterwards with a different
  ;; message, so the bug looked like a wording problem. For one carried
  ;; as a string it would simply have gone in.
  ;;
  ;; Expectations are a PostgreSQL 17 oracle's: it raises at the
  ;; assignment, with the target type's own error.
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (id int, n smallint)")
    (ok! h (str "CREATE FUNCTION f() RETURNS trigger AS $$ BEGIN"
                " NEW.n := 99999; RETURN NEW; END $$ LANGUAGE plpgsql"))
    (ok! h "CREATE TRIGGER tr BEFORE INSERT ON t FOR EACH ROW EXECUTE FUNCTION f()")
    (let [r (exec h "INSERT INTO t VALUES (1, 1)")]
      (is (= "22003" (.sqlstate r)))
      (is (re-find #"out of range" (str (err r))))
      (is (not (re-find #"(?i)invalid input syntax" (str (err r))))
          "the coercion's own verdict, not one the schema check produced later"))
    (testing "and nothing was written"
      (is (= [["0"]] (rows (ok! h "SELECT count(*) FROM t")))))
    (testing "a value that DOES fit still goes through the trigger"
      (ok! h (str "CREATE FUNCTION g() RETURNS trigger AS $$ BEGIN"
                  " NEW.n := 42; RETURN NEW; END $$ LANGUAGE plpgsql"))
      (ok! h "CREATE TABLE t2 (id int, n smallint)")
      (ok! h "CREATE TRIGGER tr2 BEFORE INSERT ON t2 FOR EACH ROW EXECUTE FUNCTION g()")
      (ok! h "INSERT INTO t2 VALUES (1, 1)")
      (is (= [["1" "42"]] (rows (ok! h "SELECT id, n FROM t2")))))))
