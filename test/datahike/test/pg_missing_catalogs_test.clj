(ns datahike.test.pg-missing-catalogs-test
  "Catalogs that were absent entirely.

   A query naming one failed with 42P01 -- `relation \"pg_operator\"
   does not exist` -- and in a script that takes the rest of the file
   with it. Eleven were missing; PostgreSQL's own regression suite names
   pg_authid 84 times, pg_operator 77 and pg_am 57.

   Four are read from the GENERATED catalog data (pg_operator.dat,
   pg_cast.dat) that type and operator resolution already read, so the
   catalog a client queries and the tables the translator resolves
   against cannot disagree. Four are fixed rows of PostgreSQL's own
   catalog. Three exist and are empty, which is the honest answer for a
   server with no range types, no planner statistics and one role.

   Counts and contents are a PostgreSQL 17 oracle's."
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
          {:keys [server]} (pg/start-server {"cat" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port* "/cat?user=x&password=x&sslmode=disable")))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(deftest the-relations-exist
  (with-open [c (jdbc)]
    (doseq [r ["pg_authid" "pg_auth_members" "pg_am" "pg_language"
               "pg_tablespace" "pg_operator" "pg_cast" "pg_sequence"
               "pg_range" "pg_statistic" "pg_shdepend"]]
      (is (some? (one c (str "SELECT count(*)::text FROM " r))) r))))

(deftest counts-match-postgresql
  (with-open [c (jdbc)]
    (testing "from the generated catalog data"
      (is (= "799" (one c "SELECT count(*)::text FROM pg_operator")))
      (is (= "229" (one c "SELECT count(*)::text FROM pg_cast"))))
    (testing "fixed rows of the catalog"
      (is (= "7" (one c "SELECT count(*)::text FROM pg_am")))
      (is (= "4" (one c "SELECT count(*)::text FROM pg_language")))
      (is (= "2" (one c "SELECT count(*)::text FROM pg_tablespace")))
      (is (= "16" (one c "SELECT count(*)::text FROM pg_authid"))
          "the login role plus PostgreSQL's 15 predefined ones"))
    (testing "empty, but present"
      (is (= "0" (one c "SELECT count(*)::text FROM pg_range")))
      (is (= "0" (one c "SELECT count(*)::text FROM pg_statistic")))
      (is (= "0" (one c "SELECT count(*)::text FROM pg_shdepend")))
      (is (= "0" (one c "SELECT count(*)::text FROM pg_auth_members"))))))

(deftest contents-match-postgresql
  (with-open [c (jdbc)]
    (is (= "brin,btree,gin,gist,hash,heap,spgist"
           (one c "SELECT string_agg(amname, ',' ORDER BY amname) FROM pg_am")))
    (is (= "c,internal,plpgsql,sql"
           (one c "SELECT string_agg(lanname, ',' ORDER BY lanname) FROM pg_language")))
    (is (= "pg_default,pg_global"
           (one c "SELECT string_agg(spcname, ',' ORDER BY spcname) FROM pg_tablespace")))
    (is (= "pg_read_all_data"
           (one c "SELECT rolname FROM pg_authid WHERE oid = 6181")))
    (testing "pg_cast resolves its function to an OID"
      ;; The generated rows carry the SIGNATURE TEXT (`int4(bool)`);
      ;; castfunc is an oid column, and storing the text failed the
      ;; column's own input function.
      (is (= "0" (one c (str "SELECT count(*)::text FROM pg_cast"
                             " WHERE castmethod = 'f' AND castfunc = 0")))
          "every function cast resolves"))))

(deftest a-sequence-appears-in-pg-sequence
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE s1")
    (is (= "1|1|1|9223372036854775807|false"
           (one c (str "SELECT seqstart::text || '|' || seqincrement::text || '|'"
                       " || seqmin::text || '|' || seqmax::text || '|'"
                       " || seqcycle::text FROM pg_sequence"))))))
