(ns datahike.test.pg-create-database-test
  "End-to-end test of SQL `CREATE DATABASE` / `DROP DATABASE` routing
   through pg-datahike's mutable registry + provisioning hooks.

   Covers:
   - `:database-template` shorthand on `start-server` builds both
     create + delete hooks from a partial datahike config template.
   - SQL `CREATE DATABASE foo` provisions a fresh in-memory store;
     a follow-up connection with `database=foo` lands on it.
   - SQL `CREATE DATABASE foo WITH KEEP_HISTORY = true` overrides
     the template's `:keep-history?` per database.
   - SQL `CREATE DATABASE foo OWNER pg ENCODING 'UTF8' …` (pg_dump
     style) silently accepts PG-only options.
   - SQL `DROP DATABASE foo` releases the conn and removes the
     registry entry.
   - SQL `DROP DATABASE IF EXISTS unknown` is a no-op (no error).
   - SQL `CREATE DATABASE` without a hook returns SQLSTATE 0A000.
   - Clojure-side `pg/add-database!` / `pg/remove-database!` /
     `pg/databases` mutate the same registry as SQL."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.database :as database])
  (:import [java.sql Connection DriverManager SQLException]))

(defn- bootstrap-conn []
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (d/connect cfg)))

(defn- jdbc-url [port db-name]
  (str "jdbc:postgresql://localhost:" port "/" db-name
       "?user=datahike&password=datahike"))

(defn- exec! [^Connection c sql]
  (with-open [stmt (.createStatement c)]
    (.execute stmt sql)))

(defn- query-rows [^Connection c sql]
  (with-open [stmt (.createStatement c)
              rs   (.executeQuery stmt sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [out []]
        (if (.next rs)
          (recur (conj out (mapv #(.getObject rs (int %)) (range 1 (inc n)))))
          out)))))

(use-fixtures :each
  (fn [f]
    (Class/forName "org.postgresql.Driver")
    (pg/reset-lock-registry!)
    (f)))

(deftest test-create-database-via-template
  (let [boot (bootstrap-conn)
        srv  (pg/start-server
              {"datahike" boot}
              {:port 0
               :database-template
               {:store {:backend :memory}
                :schema-flexibility :write
                :keep-history? false}})
        port (.getPort (:server srv))]
    (try
      ;; Connect to the bootstrap DB and issue CREATE DATABASE myapp
      (with-open [c (DriverManager/getConnection (jdbc-url port "datahike"))]
        (exec! c "CREATE DATABASE myapp"))

      ;; The new database is now in the registry.
      (is (contains? (pg/databases srv) "myapp"))

      ;; A fresh connection with database=myapp lands on it.
      (with-open [c (DriverManager/getConnection (jdbc-url port "myapp"))]
        (exec! c "CREATE TABLE t (id integer, name text)")
        (exec! c "INSERT INTO t VALUES (1, 'alice')")
        (is (= [[(int 1) "alice"]]
               (query-rows c "SELECT id, name FROM t"))))

      ;; CREATE DATABASE on an existing name returns 42P04.
      (with-open [c (DriverManager/getConnection (jdbc-url port "datahike"))]
        (let [ex (try (exec! c "CREATE DATABASE myapp")
                      (catch SQLException e e))]
          (is (instance? SQLException ex))
          (is (= "42P04" (.getSQLState ex)))))

      ;; CREATE DATABASE IF NOT EXISTS is a no-op when present.
      (with-open [c (DriverManager/getConnection (jdbc-url port "datahike"))]
        (exec! c "CREATE DATABASE IF NOT EXISTS myapp"))

      ;; DROP DATABASE removes it.
      (with-open [c (DriverManager/getConnection (jdbc-url port "datahike"))]
        (exec! c "DROP DATABASE myapp"))
      (is (not (contains? (pg/databases srv) "myapp")))

      ;; DROP DATABASE IF EXISTS unknown is a no-op.
      (with-open [c (DriverManager/getConnection (jdbc-url port "datahike"))]
        (exec! c "DROP DATABASE IF EXISTS does_not_exist"))

      ;; DROP DATABASE on missing without IF EXISTS returns 3D000.
      (with-open [c (DriverManager/getConnection (jdbc-url port "datahike"))]
        (let [ex (try (exec! c "DROP DATABASE no_such_db")
                      (catch SQLException e e))]
          (is (instance? SQLException ex))
          (is (= "3D000" (.getSQLState ex)))))

      (finally
        (.stop ^datahike.pg.PgWireServer (:server srv))
        (d/release boot)))))

(deftest test-create-database-with-options
  (let [boot (bootstrap-conn)
        srv  (pg/start-server
              {"datahike" boot}
              {:port 0
               :database-template {:store {:backend :memory}
                                   :schema-flexibility :write}})
        port (.getPort (:server srv))]
    (try
      ;; KEEP_HISTORY override
      (with-open [c (DriverManager/getConnection (jdbc-url port "datahike"))]
        (exec! c "CREATE DATABASE histdb WITH KEEP_HISTORY = true"))
      (let [conn (get @(:registry-atom srv) "histdb")]
        (is (true? (:keep-history? (:config @conn)))
            "KEEP_HISTORY = true should propagate to the conn config"))

      ;; pg_dump-style — all PG-only options silently accepted.
      (with-open [c (DriverManager/getConnection (jdbc-url port "datahike"))]
        (exec! c
               (str "CREATE DATABASE pgdump_style "
                    "OWNER postgres ENCODING 'UTF8' "
                    "LC_COLLATE 'C.UTF-8' LC_CTYPE 'C.UTF-8' "
                    "TEMPLATE 'template0'")))
      (is (contains? (pg/databases srv) "pgdump_style"))

      ;; Yugabyte-style paren form
      (with-open [c (DriverManager/getConnection (jdbc-url port "datahike"))]
        (exec! c "CREATE DATABASE paren_form WITH (KEEP_HISTORY = false)"))
      (is (contains? (pg/databases srv) "paren_form"))

      ;; Quoted identifier db name
      (with-open [c (DriverManager/getConnection (jdbc-url port "datahike"))]
        (exec! c "CREATE DATABASE \"my-db\""))
      (is (contains? (pg/databases srv) "my-db"))

      ;; Unknown option is a hard error (42601 syntax_error).
      (with-open [c (DriverManager/getConnection (jdbc-url port "datahike"))]
        (let [ex (try (exec! c "CREATE DATABASE bad WITH NONSENSE = 'x'")
                      (catch SQLException e e))]
          (is (instance? SQLException ex))
          (is (= "42601" (.getSQLState ex)))))

      (finally
        (.stop ^datahike.pg.PgWireServer (:server srv))
        (d/release boot)))))

(deftest test-create-database-without-hook
  (let [boot (bootstrap-conn)
        srv  (pg/start-server {"datahike" boot} {:port 0})  ;; no template
        port (.getPort (:server srv))]
    (try
      (with-open [c (DriverManager/getConnection (jdbc-url port "datahike"))]
        (let [ex (try (exec! c "CREATE DATABASE foo")
                      (catch SQLException e e))]
          (is (instance? SQLException ex))
          (is (= "0A000" (.getSQLState ex))
              "Without :on-create-database / :database-template, CREATE DATABASE returns 0A000")))
      (finally
        (.stop ^datahike.pg.PgWireServer (:server srv))
        (d/release boot)))))

(deftest test-clojure-side-mutation
  (let [boot (bootstrap-conn)
        srv  (pg/start-server {"datahike" boot} {:port 0})
        port (.getPort (:server srv))
        cfg  {:store {:backend :memory :id (java.util.UUID/randomUUID)}
              :schema-flexibility :write
              :keep-history? false}]
    (d/create-database cfg)
    (let [extra-conn (d/connect cfg)]
      (try
        (is (= #{"datahike"} (pg/databases srv)))

        ;; Add via Clojure API, then connect via JDBC.
        (pg/add-database! srv "extra" extra-conn)
        (is (= #{"datahike" "extra"} (pg/databases srv)))

        (with-open [c (DriverManager/getConnection (jdbc-url port "extra"))]
          (exec! c "CREATE TABLE t (id integer)")
          (exec! c "INSERT INTO t VALUES (42)")
          (is (= [[(int 42)]] (query-rows c "SELECT id FROM t"))))

        (pg/remove-database! srv "extra")
        (is (= #{"datahike"} (pg/databases srv)))
        (finally
          (.stop ^datahike.pg.PgWireServer (:server srv))
          (d/release extra-conn)
          (d/release boot)
          (d/delete-database cfg))))))

(deftest test-database-parser-unit
  (testing "tokenize handles strings, idents, numbers, booleans, parens, equals"
    (is (= [[:ident "CREATE"] [:ident "DATABASE"] [:ident "foo"]
            [:ident "WITH"] [:ident "KEEP_HISTORY"] [:eq "="] [:bool "true"]]
           (database/tokenize "CREATE DATABASE foo WITH KEEP_HISTORY = true"))))

  (testing "parse-create-database extracts name + options"
    (is (= {:db-name "foo" :if-not-exists? false
            :options [["backend" "memory"] ["keep_history" true]]}
           (database/parse-create-database
            (database/tokenize "CREATE DATABASE foo WITH BACKEND = 'memory' KEEP_HISTORY = true")))))

  (testing "parse-create-database accepts paren form"
    (is (= {:db-name "foo" :if-not-exists? false
            :options [["backend" "memory"]]}
           (database/parse-create-database
            (database/tokenize "CREATE DATABASE foo WITH (BACKEND = 'memory')")))))

  (testing "parse-create-database recognises IF NOT EXISTS"
    (is (= {:db-name "foo" :if-not-exists? true :options []}
           (database/parse-create-database
            (database/tokenize "CREATE DATABASE IF NOT EXISTS foo")))))

  (testing "parse-drop-database recognises IF EXISTS"
    (is (= {:db-name "foo" :if-exists? true}
           (database/parse-drop-database
            (database/tokenize "DROP DATABASE IF EXISTS foo")))))

  (testing "options->config silently NOTICEs PG-only options"
    (let [{:keys [config notices]}
          (database/options->config
           {:store {:backend :memory} :schema-flexibility :write}
           [["owner" "postgres"] ["encoding" "UTF8"]])]
      (is (= {:store {:backend :memory} :schema-flexibility :write} config))
      (is (= 2 (count notices)))))

  (testing "options->config rejects unknown options"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"unknown option"
         (database/options->config {} [["nonsense" "x"]])))))
