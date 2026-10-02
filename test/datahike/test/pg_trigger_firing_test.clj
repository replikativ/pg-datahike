(ns datahike.test.pg-trigger-firing-test
  "Five ways a trigger did not fire, or fired with the wrong record
   bound, found by review.

   Driven over pgjdbc rather than the in-process handler. A trigger
   body runs its own statements through `*statement-handler*`, and that
   is bound by the wire layer -- so the in-process path, which every
   other trigger test uses, exercises a different machine and loses the
   body's writes. The same lesson as the cursor tests.

   Expectations are a PostgreSQL 17.7 oracle's."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0 :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"tg" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port* "/tg?user=x&password=x&sslmode=disable")))

(defn- exec! [^Connection c sql]
  (with-open [s (.createStatement c)] (.execute s sql)))

(defn- col1 [^Connection c sql]
  (with-open [s (.createStatement c) r (.executeQuery s sql)]
    (loop [acc []] (if (.next r) (recur (conj acc (.getString r 1))) acc))))

(defn- state-of [^Connection c sql]
  (try (exec! c sql) nil (catch SQLException e (.getSQLState e))))

(defn- message-of [^Connection c sql]
  (try (exec! c sql) nil (catch SQLException e (.getMessage e))))

(defn- trigfn! [^Connection c nm body]
  (exec! c (str "CREATE OR REPLACE FUNCTION " nm "() RETURNS trigger "
                "LANGUAGE plpgsql AS $$ " body " $$")))

(deftest the-unassigned-record-is-null-not-an-error
  ;; `OLD` in an INSERT trigger and `NEW` in a DELETE one are
  ;; unassigned; PostgreSQL gives NULL for their fields. Nothing was
  ;; bound, so the name fell through to SQL resolution and the body
  ;; died with `column "old" does not exist` -- which breaks the
  ;; one-function-for-every-event shape PostgreSQL's own documentation
  ;; uses. The DELETE case was worse than an error: NEW was bound to
  ;; OLD, so a body branching on NEW read OLD's values.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE olog (m text)")
    (exec! c "CREATE TABLE o1 (a int)")
    (trigfn! c "g" (str "BEGIN INSERT INTO olog VALUES ('op='||TG_OP||' when='||TG_WHEN"
                        "||' new='||coalesce(NEW.a::text,'-')"
                        "||' old='||coalesce(OLD.a::text,'-'));"
                        " RETURN coalesce(NEW, OLD); END"))
    (exec! c (str "CREATE TRIGGER o_b BEFORE INSERT OR UPDATE OR DELETE ON o1 "
                  "FOR EACH ROW EXECUTE FUNCTION g()"))
    (exec! c (str "CREATE TRIGGER o_a AFTER INSERT OR UPDATE OR DELETE ON o1 "
                  "FOR EACH ROW EXECUTE FUNCTION g()"))
    (exec! c "INSERT INTO o1 VALUES (1)")
    (exec! c "UPDATE o1 SET a = 2")
    (exec! c "DELETE FROM o1")
    (is (= ["op=INSERT when=BEFORE new=1 old=-"
            "op=INSERT when=AFTER new=1 old=-"
            "op=UPDATE when=BEFORE new=2 old=1"
            "op=UPDATE when=AFTER new=2 old=1"
            "op=DELETE when=BEFORE new=- old=2"
            "op=DELETE when=AFTER new=- old=2"]
           (col1 c "SELECT m FROM olog")))
    (testing "and the DELETE went through. `RETURN coalesce(NEW, OLD)`
              runs through the SQL evaluator, which renders the record
              marker to TEXT, so the marker has to be recognised in
              both spellings -- otherwise the return reads as NULL and
              a BEFORE DELETE trigger SUPPRESSES the row"
      (is (= ["0"] (col1 c "SELECT count(*) FROM o1"))))))

(deftest insert-returning-fires-after-triggers
  ;; They lived only in the non-RETURNING branch, so `INSERT …
  ;; RETURNING` ran none -- while `UPDATE … RETURNING` and
  ;; `DELETE … RETURNING` did, so this was INSERT alone.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE rlog (m text)")
    (exec! c "CREATE TABLE r1 (a int)")
    (trigfn! c "rf" "BEGIN INSERT INTO rlog VALUES (TG_NAME||' '||TG_LEVEL); RETURN NEW; END")
    (exec! c "CREATE TRIGGER r_ai AFTER INSERT ON r1 FOR EACH ROW EXECUTE FUNCTION rf()")
    (exec! c "CREATE TRIGGER r_si AFTER INSERT ON r1 FOR EACH STATEMENT EXECUTE FUNCTION rf()")
    (is (= ["1"] (col1 c "INSERT INTO r1 VALUES (1) RETURNING a")))
    (is (= ["r_ai ROW" "r_si STATEMENT"] (col1 c "SELECT m FROM rlog ORDER BY m")))))

(deftest update-of-column-is-compared-by-every-timing
  ;; `matching-triggers` takes the statement's target column list, and
  ;; the AFTER ROW and STATEMENT call sites passed a literal nil -- so
  ;; the list was never consulted and an `AFTER UPDATE OF b` trigger
  ;; fired on `UPDATE t SET a = …`. Both UPDATE paths had it.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE wlog (m text)")
    (exec! c "CREATE TABLE w1 (a int, b int)")
    (trigfn! c "wf" "BEGIN INSERT INTO wlog VALUES (TG_NAME); RETURN coalesce(NEW, OLD); END")
    (doseq [[n when lvl] [["w_b" "BEFORE" "ROW"] ["w_sb" "BEFORE" "STATEMENT"]
                          ["w_a" "AFTER" "ROW"] ["w_sa" "AFTER" "STATEMENT"]]]
      (exec! c (str "CREATE TRIGGER " n " " when " UPDATE OF b ON w1 FOR EACH " lvl
                    " EXECUTE FUNCTION wf()")))
    (exec! c "INSERT INTO w1 VALUES (1,1),(2,2)")
    (exec! c "UPDATE w1 SET a = a + 10")
    (is (= [] (col1 c "SELECT m FROM wlog"))
        "a statement that touches only `a` fires none of them")
    (exec! c "UPDATE w1 SET b = b + 10")
    (is (= ["w_a" "w_a" "w_b" "w_b" "w_sa" "w_sb"]
           (col1 c "SELECT m FROM wlog ORDER BY m")))))

(deftest on-conflict-fires-before-insert-for-the-proposed-row
  ;; The ON CONFLICT path handed `fire-row-triggers` raw tx ops rather
  ;; than entity maps, so its `(if-not (map? entry) entry …)` guard
  ;; passed every one through and NOTHING fired.
  ;;
  ;; This is the half PostgreSQL does first. The UPDATE-side triggers
  ;; of the DO UPDATE branch are still not fired -- doc/review-backlog.md.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE clog (m text)")
    (exec! c "CREATE TABLE c1 (a int PRIMARY KEY, b int)")
    (trigfn! c "cf" (str "BEGIN INSERT INTO clog VALUES (TG_WHEN||' '||TG_OP||' a='||NEW.a);"
                         " NEW.b := NEW.b + 500; RETURN NEW; END"))
    (exec! c "CREATE TRIGGER c_bi BEFORE INSERT ON c1 FOR EACH ROW EXECUTE FUNCTION cf()")
    (exec! c "INSERT INTO c1 VALUES (1,1)")
    (is (= ["501"] (col1 c "SELECT b FROM c1")))
    (exec! c "DELETE FROM clog")
    (exec! c "INSERT INTO c1 VALUES (1,7) ON CONFLICT (a) DO UPDATE SET b = excluded.b")
    (is (= ["BEFORE INSERT a=1"] (col1 c "SELECT m FROM clog")))
    (is (= ["507"] (col1 c "SELECT b FROM c1")))))

(deftest truncate-triggers-fire
  ;; They were parsed, stored, and reported in pg_trigger with the
  ;; right tgtype -- and never fired, because `matching-triggers` was
  ;; only ever called with :insert/:update/:delete.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE tlog (m text)")
    (exec! c "CREATE TABLE tr1 (a int)")
    (trigfn! c "trf" "BEGIN INSERT INTO tlog VALUES (TG_WHEN||' '||TG_OP); RETURN NULL; END")
    (exec! c "CREATE TRIGGER tr_b BEFORE TRUNCATE ON tr1 FOR EACH STATEMENT EXECUTE FUNCTION trf()")
    (exec! c "CREATE TRIGGER tr_a AFTER TRUNCATE ON tr1 FOR EACH STATEMENT EXECUTE FUNCTION trf()")
    (exec! c "INSERT INTO tr1 VALUES (1),(2)")
    (exec! c "TRUNCATE tr1")
    (is (= ["AFTER TRUNCATE" "BEFORE TRUNCATE"] (col1 c "SELECT m FROM tlog ORDER BY m")))
    (is (= ["0"] (col1 c "SELECT count(*) FROM tr1")))))

(deftest instead-of-on-a-table-is-refused
  ;; INSTEAD OF is for views (CreateTrigger, trigger.c). On a table
  ;; this was ACCEPTED, and `matching-triggers` then matched only
  ;; :before/:after -- so the trigger sat in the catalog and never
  ;; fired. A silent dead trigger, which is worse than the refusal.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE iv (a int)")
    (trigfn! c "ivf" "BEGIN RETURN NEW; END")
    (let [sql (str "CREATE TRIGGER iv_t INSTEAD OF INSERT ON iv "
                   "FOR EACH ROW EXECUTE FUNCTION ivf()")]
      (is (= "42809" (state-of c sql)))
      (is (str/includes? (message-of c sql) "Tables cannot have INSTEAD OF triggers")))))

;; ============================================================================
;; DO blocks, over the wire. A DO body's statements go through
;; `*statement-handler*` for the same reason a trigger body's do, so
;; these belong here rather than with the in-process plpgsql tests --
;; and transaction control cannot be spoken for at all without a wire
;; layer, since the implicit transaction is the wire layer's.
;; ============================================================================

(deftest commit-inside-do-cannot-end-the-callers-transaction
  ;; `BEGIN; … DO $$ … COMMIT; … $$; … ROLLBACK;` committed the
  ;; CALLER's transaction, so the client's ROLLBACK undid nothing and
  ;; every row survived. PostgreSQL raises 2D000, `invalid transaction
  ;; termination` (SPI_commit).
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE dt (a int)")
    (.setAutoCommit c false)
    (exec! c "INSERT INTO dt VALUES (1)")
    (is (= "2D000" (state-of c "DO $$ BEGIN INSERT INTO dt VALUES (2); COMMIT; END $$")))
    (.rollback c)
    (.setAutoCommit c true)
    (is (= [] (col1 c "SELECT a FROM dt ORDER BY a"))
        "the ROLLBACK undoes everything, including what the DO wrote")))

(deftest a-do-block-in-autocommit-may-still-manage-transactions
  ;; The other half, which already worked and must keep working:
  ;; outside an explicit transaction block a DO may COMMIT and start a
  ;; new transaction, and its ROLLBACK discards its own writes.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE dt2 (a int)")
    (exec! c "DO $$ BEGIN INSERT INTO dt2 VALUES (1); COMMIT; INSERT INTO dt2 VALUES (2); END $$")
    (is (= ["1" "2"] (col1 c "SELECT a FROM dt2 ORDER BY a")))
    (exec! c "DO $$ BEGIN INSERT INTO dt2 VALUES (3); ROLLBACK; END $$")
    (is (= ["1" "2"] (col1 c "SELECT a FROM dt2 ORDER BY a")))))

(deftest do-refuses-what-create-function-refuses
  ;; A DO block parsed its body directly instead of through the gate
  ;; CREATE FUNCTION uses, so a construct the executor cannot run was
  ;; silently DROPPED. For an EXCEPTION handler the difference is not
  ;; cosmetic: the error the handler was written to catch escaped to
  ;; the client, as a division-by-zero rather than a refusal.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE de (a int)")
    (let [body (str "BEGIN INSERT INTO de VALUES (1/0); "
                    "EXCEPTION WHEN division_by_zero THEN "
                    "INSERT INTO de VALUES (99); END")]
      (is (= "0A000" (state-of c (str "DO $$ " body " $$"))))
      (is (str/includes? (message-of c (str "DO $$ " body " $$"))
                         "exception-handler is not supported"))
      (testing "which is the answer CREATE FUNCTION already gave"
        (is (= "0A000" (state-of c (str "CREATE FUNCTION fe() RETURNS void "
                                        "LANGUAGE plpgsql AS $$ " body " $$"))))))
    (is (= [] (col1 c "SELECT a FROM de")))))

(deftest the-catalog-can-reconstruct-a-trigger
  ;; `pg_get_triggerdef` was a `(constantly :__null__)` stub, so
  ;; anything reconstructing DDL from the catalog lost every trigger.
  ;; `pg_class.relhastriggers` was NULL while `pg_tables.hastriggers`
  ;; was right -- and `\d` reads the former. `tgattr` was absent, so an
  ;; `UPDATE OF` column list was invisible.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE kt (id int PRIMARY KEY, a int)")
    (trigfn! c "ktf" "BEGIN RETURN NEW; END")
    (exec! c (str "CREATE TRIGGER kt_t AFTER UPDATE OF a ON kt "
                  "FOR EACH ROW EXECUTE FUNCTION ktf()"))
    (is (= ["t"] (col1 c "SELECT relhastriggers FROM pg_class WHERE relname = 'kt'")))
    (is (= ["2"] (col1 c "SELECT tgattr FROM pg_trigger WHERE tgrelid = 'kt'::regclass")))
    (is (= [(str "CREATE TRIGGER kt_t AFTER UPDATE OF a ON public.kt "
                 "FOR EACH ROW EXECUTE FUNCTION ktf()")]
           (col1 c (str "SELECT pg_get_triggerdef(oid) FROM pg_trigger "
                        "WHERE tgrelid = 'kt'::regclass"))))
    (testing "a statement-level multi-event trigger, with the events in
              PostgreSQL's own order rather than the order written"
      (exec! c (str "CREATE TRIGGER kt_s AFTER DELETE OR INSERT ON kt "
                    "FOR EACH STATEMENT EXECUTE FUNCTION ktf()"))
      (is (= [(str "CREATE TRIGGER kt_s AFTER INSERT OR DELETE ON public.kt "
                   "FOR EACH STATEMENT EXECUTE FUNCTION ktf()")]
             (col1 c (str "SELECT pg_get_triggerdef(oid) FROM pg_trigger "
                          "WHERE tgname = 'kt_s'")))))))
