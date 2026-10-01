(ns datahike.test.pg-session-catalogs-test
  "`pg_prepared_statements`, `pg_cursors`, and the views over
   facilities this server does not have.

   These two are unlike every other catalog here: they are not derived
   from the database. A PREPAREd statement and a DECLAREd cursor belong
   to one connection and die with it, so the rows come from the session
   -- which is why the catalog layer needed to be given it.

   `prepare.sql` is named for the first and `sysviews.sql` queries
   eighteen system views, sixteen of which did not exist. A missing one
   is a 42P01 that takes the rest of the file with it.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0 :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"sc" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port* "/sc?user=x&password=x&sslmode=disable")))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [out []]
        (if (.next rs)
          (recur (conj out (mapv #(.getString rs (int %)) (range 1 (inc n)))))
          out)))))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(deftest prepared-statements-are-this-session's
  (with-open [c (jdbc)]
    (is (= "0" (one c "SELECT count(*) FROM pg_prepared_statements")))
    (exec! c "PREPARE q1 AS SELECT 1 AS a")
    (exec! c "PREPARE q2(int, text) AS SELECT $1::int AS i, $2::text AS t")
    (testing "parameter and result types are the ones Describe reports,
              spelled the way regtype prints them"
      (is (= [["q1" "{}" "{integer}"]
              ["q2" "{integer,text}" "{integer,text}"]]
             (rows c (str "SELECT name, parameter_types::text, result_types::text "
                          "FROM pg_prepared_statements ORDER BY name")))))
    (testing "DEALLOCATE removes it"
      (exec! c "DEALLOCATE q1")
      (is (= [["q2"]] (rows c "SELECT name FROM pg_prepared_statements")))
      (exec! c "DEALLOCATE ALL")
      (is (= "0" (one c "SELECT count(*) FROM pg_prepared_statements"))))
    (testing "another connection sees none of it"
      (exec! c "PREPARE mine AS SELECT 1")
      (with-open [c2 (jdbc)]
        (is (= "0" (one c2 "SELECT count(*) FROM pg_prepared_statements")))))))

(deftest the-statement-column-is-the-submitted-query-string
  ;; PostgreSQL reports `debug_query_string`: the query string as the
  ;; client submitted it, NOT the template EXECUTE runs. pgjdbc sends
  ;; one statement per message, so here that is the PREPARE itself --
  ;; and a real PostgreSQL answers the same thing to the same client.
  ;;
  ;; The case this cannot show is a multi-statement Simple Query, where
  ;; PostgreSQL reports the WHOLE string, siblings and semicolon
  ;; included, for every statement in it. That is why the wire layer
  ;; hands the string over at all -- `execute` is given a Simple Query
  ;; already split. Checked over psql against a PostgreSQL 17 oracle,
  ;; where `DEALLOCATE ALL; PREPARE z …; PREPARE y …;` reports that
  ;; whole string for both z and y, identically on both sides.
  (with-open [c (jdbc)]
    (exec! c "PREPARE z AS SELECT 1")
    (is (= [["z" "PREPARE z AS SELECT 1"]]
           (rows c "SELECT name, statement FROM pg_prepared_statements ORDER BY name")))))

(deftest a-statement-with-no-result-columns-still-prepares
  ;; The view asks the plan for its result OIDs, and an INSERT -- which
  ;; is what a PREPAREd statement usually is -- describes as nothing at
  ;; all. Reading a field off that threw INSIDE PREPARE, so the
  ;; statement was never stored and the EXECUTE that followed answered
  ;; nil, a long way from the cause.
  ;; (The EXECUTE is not driven here: SQL-level EXECUTE over the
  ;; EXTENDED protocol closes the connection, which is a separate
  ;; pre-existing bug and reproduces on main without any of this.)
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE pz (id int PRIMARY KEY, v text)")
    (exec! c "PREPARE ins AS INSERT INTO pz VALUES ($1, $2)")
    (is (= [["ins" "{}"]]
           (rows c (str "SELECT name, result_types::text "
                        "FROM pg_prepared_statements WHERE name = 'ins'"))))))

(deftest a-prepare-with-parameters-survives-the-extended-protocol
  ;; `$1` in a PREPARE is a placeholder in a TEMPLATE, bound later by
  ;; EXECUTE. Over the extended protocol the PREPARE arrives with its
  ;; own (empty) bind list, and that binding was still in scope when
  ;; the template was parsed -- so `$1` resolved against it and the
  ;; statement died with an IndexOutOfBoundsException. psql never saw
  ;; it, because the simple path has no bind list at all.
  (with-open [c (jdbc)]
    (exec! c "PREPARE qp(int, text) AS SELECT $1::int AS i, $2::text AS t")
    (is (= [["{integer,text}"]]
           (rows c (str "SELECT parameter_types::text FROM pg_prepared_statements "
                        "WHERE name = 'qp'"))))))

(deftest cursors-report-what-they-can-do
  (with-open [c (jdbc)]
    (exec! c "BEGIN")
    (exec! c "DECLARE c1 CURSOR FOR SELECT 1")
    ;; Not what was written: a FETCH here only scans forward and only
    ;; in text, so SCROLL and BINARY would still report false.
    (is (= [["c1" "f" "f" "f"]]
           (rows c (str "SELECT name, is_holdable, is_binary, is_scrollable "
                        "FROM pg_cursors"))))
    (exec! c "COMMIT")))

(deftest the-views-with-nothing-to-report-exist-and-are-empty
  ;; Empty is the truth, not a convenience: there is no WAL, no 2PC, no
  ;; postgresql.conf or pg_hba.conf, and no extension mechanism. A view
  ;; that invented rows to look more like PostgreSQL would be worse
  ;; than the 42P01 it replaces -- but so is a 42P01 that stops a
  ;; client's introspection dead.
  (with-open [c (jdbc)]
    (doseq [v ["pg_prepared_xacts" "pg_available_extensions"
               "pg_available_extension_versions" "pg_file_settings"
               "pg_hba_file_rules" "pg_ident_file_mappings"
               "pg_stat_wal" "pg_stat_slru" "pg_stat_wal_receiver"]]
      (is (= "0" (one c (str "SELECT count(*) FROM " v))) v)
      (is (= "t" (one c (str "SELECT count(*) >= 0 AS ok FROM " v))) v))))
