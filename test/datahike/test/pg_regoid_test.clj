(ns datahike.test.pg-regoid-test
  "Routine identity input/output, checked against PostgreSQL's regproc.c."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]
           [java.io StringReader]
           [org.postgresql.core BaseConnection]
           [org.postgresql.copy CopyManager]))

(def ^:dynamic *port* nil)

(use-fixtures :each
  (fn [f]
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
               :schema-flexibility :write :keep-history? false :max-string-length 0}]
      (d/create-database cfg)
      (let [conn (d/connect cfg)
            srv (pg/start-server {"regoid" conn} {:port 0 :host "127.0.0.1"})]
        (try (binding [*port* (.getPort (:server srv))] (f))
             (finally (pg/stop-server srv) (d/release conn) (d/delete-database cfg)))))))

(defn- connect ^Connection [simple?]
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/regoid?user=x&password=x&sslmode=disable&binaryTransfer=false"
        (when simple? "&preferQueryMode=simple"))))

(defn- rows [rs]
  (let [n (.getColumnCount (.getMetaData ^java.sql.ResultSet rs))]
    (loop [out []]
      (if (.next ^java.sql.ResultSet rs)
        (recur (conj out (mapv #(.getString ^java.sql.ResultSet rs (int %))
                               (range 1 (inc n))))) out))))

(defn- query [^Connection c q]
  (try
    (with-open [st (.prepareStatement c q) rs (.executeQuery st)]
      {:rows (rows rs)})
    (catch SQLException e {:sqlstate (.getSQLState e)})))

(defn- execute! [^Connection c q]
  (with-open [st (.createStatement c)] (.execute st q)))

(deftest scalar-corpus-over-both-protocols
  (let [corpus (:cases (edn/read-string (slurp "test/resources/regoid_corpus.edn")))]
    (is (= 52 (count corpus)))
    (doseq [simple? [true false]]
      (with-open [c (connect simple?)]
        (doseq [[q expected] corpus]
          (is (= expected (query c q)) (str (if simple? "simple " "extended ") q)))))))

(deftest stored-routine-identities-and-catalog
  (doseq [simple? [true false]]
    (with-open [c (connect simple?)]
      (execute! c "CREATE FUNCTION rg_ident(a integer) RETURNS integer LANGUAGE sql AS 'SELECT a'")
      (is (= {:rows [["rg_ident" "rg_ident(integer)" "t"]]}
             (query c "SELECT 'rg_ident'::regproc::text, 'rg_ident(integer)'::regprocedure::text, 'rg_ident'::regproc::oid = 'rg_ident(integer)'::regprocedure::oid")))
      (is (= {:rows [["rg_ident" "1" "23"]]}
             (query c "SELECT proname, pronargs, proargtypes FROM pg_proc WHERE oid = 'rg_ident(integer)'::regprocedure::oid")))
      (execute! c "CREATE FUNCTION rg_ident(a text) RETURNS text LANGUAGE sql AS 'SELECT a'")
      (is (= {:sqlstate "42725"} (query c "SELECT 'rg_ident'::regproc")))
      (is (= {:rows [["public.rg_ident"]]}
             (query c "SELECT 'rg_ident(integer)'::regprocedure::oid::regproc")))
      (execute! c "DROP FUNCTION rg_ident(text)")
      (execute! c "DROP FUNCTION rg_ident(integer)")
      (is (= {:rows [["t"]]} (query c "SELECT to_regproc('rg_ident') IS NULL"))))))

(deftest prepared-lookup-observes-ddl-search-path-and-rollback
  (with-open [c (connect false)]
    (execute! c "CREATE FUNCTION rg_live() RETURNS integer LANGUAGE sql AS 'SELECT 1'")
    (with-open [st (.prepareStatement c "SELECT to_regproc(?)::text")]
      (.setString st 1 "rg_live")
      (dotimes [_ 6]
        (with-open [rs (.executeQuery st)] (is (= [["rg_live"]] (rows rs)))))
      (execute! c "SET search_path TO pg_catalog")
      (with-open [rs (.executeQuery st)] (is (= [[nil]] (rows rs))))
      (execute! c "SET search_path TO public")
      (.setAutoCommit c false)
      (execute! c "DROP FUNCTION rg_live()")
      (with-open [rs (.executeQuery st)] (is (= [[nil]] (rows rs))))
      (.rollback c)
      (with-open [rs (.executeQuery st)] (is (= [["rg_live"]] (rows rs))))
      (.setAutoCommit c true)
      (execute! c "DROP FUNCTION rg_live()")
      (with-open [rs (.executeQuery st)] (is (= [[nil]] (rows rs)))))))

(deftest routine-columns-and-arrays
  (with-open [c (connect false)]
    (execute! c "CREATE TABLE rg_values (p regproc, f regprocedure)")
    (execute! c "INSERT INTO rg_values VALUES ('now', 'abs(integer)')")
    (is (= {:rows [["now" "abs(integer)"]]}
           (query c "SELECT p, f FROM rg_values")))
    (is (= {:rows [["now" "abs(integer)"]]}
           (query c "SELECT p::text, f::text FROM (SELECT p, f FROM rg_values) AS r")))
    (is (= {:rows [["{now,-}" "{now(),abs(integer)}"]]}
           (query c "SELECT '{now,-}'::regproc[], '{now(),abs(integer)}'::regprocedure[]")))
    (execute! c "CREATE TABLE rg_arrays (p regproc[])")
    (execute! c "INSERT INTO rg_arrays VALUES ('{now,-}')")
    (is (= {:rows [["{now,-}"]]}
           (query c "SELECT p FROM rg_arrays")))
    (is (= {:rows [["1299"]]}
           (query c "SELECT x::oid FROM unnest(ARRAY['now']::regproc[]) AS r(x)")))
    (is (= {:sqlstate "42883"}
           (query c "SELECT ARRAY['rg_unknown']::regproc[]")))
    (execute! c "CREATE FUNCTION rg_array_fn() RETURNS integer LANGUAGE sql AS 'SELECT 1'")
    (let [id (get-in (query c "SELECT 'rg_array_fn'::regproc::oid") [:rows 0 0])]
      (execute! c "INSERT INTO rg_arrays VALUES (ARRAY['rg_array_fn']::regproc[])")
      (execute! c "DROP FUNCTION rg_array_fn()")
      (is (= {:rows [[(str "{" id "}")]]}
             (query c "SELECT p FROM rg_arrays WHERE p <> '{1299,0}'::regproc[]"))))))

(deftest quoted-routine-name
  (with-open [c (connect false)]
    (execute! c "CREATE FUNCTION \"Rg.Quote\"() RETURNS integer LANGUAGE sql AS 'SELECT 1'")
    (is (= {:rows [["\"Rg.Quote\""]]}
           (query c "SELECT '\"Rg.Quote\"'::regproc")))))

(deftest type-directed-output-and-unnest
  (with-open [c (connect false)]
    (is (= {:rows [["{now}"]]}
           (query c "SELECT array_agg(x) FROM (VALUES ('now'::regproc)) s(x)")))
    (is (= {:rows [["[\"now\"]"]]}
           (query c "SELECT to_json(array_agg(x)) FROM (VALUES ('now'::regproc)) s(x)")))
    (is (= {:rows [["1299" "regproc"] ["0" "regproc"]]}
           (query c "SELECT x::oid, pg_typeof(x)::text FROM unnest('{now,-}'::regproc[]) WITH ORDINALITY s(x,n) ORDER BY n")))
    (is (= {:rows [["'now'" "'now'" "[\"now\"]" "\"now\""]]}
           (query c "SELECT quote_literal('now'::regproc), quote_nullable('now'::regproc), json_build_array('now'::regproc), to_json('now'::regproc)")))))

(deftest copy-uses-the-statement-catalog-and-search-path
  (with-open [c (connect false)]
    (execute! c "CREATE FUNCTION rg_copy_fn() RETURNS integer LANGUAGE sql AS 'SELECT 1'")
    (execute! c "CREATE TABLE rg_copy (p regproc, a regproc[])")
    (let [copy (CopyManager. ^BaseConnection (.unwrap c BaseConnection))]
      ;; No trailing newline: the final row is processed at CopyDone.
      (is (= 1 (.copyIn copy "COPY rg_copy FROM STDIN"
                        (StringReader. "rg_copy_fn\t{rg_copy_fn,-}"))))
      (execute! c "SET search_path TO pg_catalog")
      (is (= "42883"
             (try (.copyIn copy "COPY public.rg_copy FROM STDIN"
                           (StringReader. "rg_copy_fn\t{rg_copy_fn,-}"))
                  nil
                  (catch SQLException e (.getSQLState e)))))
      (is (= {:rows [["1"]]} (query c "SELECT count(*) FROM public.rg_copy")))
      (is (= 1 (.copyIn copy "COPY public.rg_copy FROM STDIN"
                        (StringReader. "public.rg_copy_fn\t{public.rg_copy_fn,-}"))))
      (execute! c "SET search_path TO public")
      (is (= {:rows [["rg_copy_fn" "{rg_copy_fn,-}"] ["rg_copy_fn" "{rg_copy_fn,-}"]]}
             (query c "SELECT p, a FROM rg_copy"))))))

(deftest typed-quoting-preserves-deferred-effects-and-null-behavior
  (with-open [c (connect false)]
    (execute! c "CREATE SEQUENCE rg_quote_sequence")
    (is (= {:rows [["'1'"]]}
           (query c "SELECT quote_literal(nextval('rg_quote_sequence'))")))
    (is (= {:rows [["'2'"]]}
           (query c "SELECT quote_nullable(nextval('rg_quote_sequence'))")))
    (is (= {:rows [["3"]]}
           (query c "SELECT nextval('rg_quote_sequence')")))
    (is (= {:rows [[nil "NULL"]]}
           (query c "SELECT quote_literal(NULL), quote_nullable(NULL)")))))

(deftest builtin-oids-have-their-exact-signatures
  (with-open [c (connect false)]
    (is (= {:rows [["abs" "23" "23"]]}
           (query c "SELECT proname, proargtypes, prorettype FROM pg_proc WHERE oid = 'abs(integer)'::regprocedure::oid")))
    (is (= {:rows [["t"]]}
           (query c "SELECT count(*) = count(DISTINCT oid) FROM pg_proc")))
    (is (= {:rows [["regproc" "4"]]}
           (query c "SELECT typname, typlen FROM pg_type WHERE oid = 24")))))
