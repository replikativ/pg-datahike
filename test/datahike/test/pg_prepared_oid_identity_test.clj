(ns datahike.test.pg-prepared-oid-identity-test
  "Prepared OID identities and analysis invalidation, checked against PG 17.10."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager PreparedStatement SQLException Statement]
           [org.postgresql PGStatement]))

(def ^:dynamic *port* nil)
(def ^:dynamic *conn* nil)

(use-fixtures :each
  (fn [f]
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
               :schema-flexibility :write :keep-history? false :max-string-length 0}]
      (d/create-database cfg)
      (let [conn (d/connect cfg)
            srv (pg/start-server {"prepared_oids" conn} {:port 0 :host "127.0.0.1"})]
        (try (binding [*port* (.getPort (:server srv)) *conn* conn] (f))
             (finally (pg/stop-server srv) (d/release conn) (d/delete-database cfg)))))))

(defn- connect ^Connection [mode]
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/prepared_oids?user=x&password=x&sslmode=disable&binaryTransfer=false"
        (case mode :sql "&preferQueryMode=simple" :unnamed "&prepareThreshold=0" "&prepareThreshold=5"))))

(defn- rows [^java.sql.ResultSet rs]
  (let [n (.getColumnCount (.getMetaData rs))]
    (loop [result []]
      (if (.next rs)
        (recur (conj result (mapv #(.getString rs (int %)) (range 1 (inc n)))))
        result))))

(defn- execute-statement [^Statement st sql]
  (try
    (let [result? (if sql (.execute st ^String sql) (.execute ^PreparedStatement st))]
      (if result?
        (with-open [rs (.getResultSet st)] {:rows (rows rs)})
        {:updated (.getUpdateCount st)}))
    (catch SQLException e {:sqlstate (.getSQLState e)})))

(defn- query [^Connection c sql]
  (with-open [st (.createStatement c)] (execute-statement st sql)))

(defn- execute! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st ^String sql)))

(defn- literal [s]
  (str "'" (str/replace (str s) "'" "''") "'"))

(defn- prepare [^Connection c mode name sql]
  (if (= mode :sql)
    (do (execute! c (str "PREPARE " name " AS " sql))
        {:run #(query c (str "EXECUTE " name))
         :close #(execute! c (str "DEALLOCATE " name))})
    (let [st (.prepareStatement c ^String sql)]
      (.setPrepareThreshold ^PGStatement st 5)
      {:run #(execute-statement st nil) :close #(.close st)})))

(defn- run [p] ((:run p)))
(defn- close! [p] ((:close p)))

(defn- warm! [p expected]
  ;; Cross pgjdbc's named-Parse threshold, exercising both initial executions
  ;; and reuse of an actual server-side prepared statement.
  (dotimes [_ 6] (is (= expected (run p)))))

(defn- create-object! [c kind name]
  (execute! c (case kind
                "regtype" (str "CREATE TYPE " name " AS ENUM ('a')")
                "regclass" (str "CREATE TABLE " name " (v integer)")
                "regproc" (str "CREATE FUNCTION " name "() RETURNS integer LANGUAGE sql AS 'SELECT 1'"))))

(defn- rename-object! [c kind before after]
  (execute! c (case kind
                "regtype" (str "ALTER TYPE " before " RENAME TO " after)
                "regproc" (let [eid (d/q '[:find ?e . :in $ ?name
                                           :where [?e :datahike.pg.function/name ?name]]
                                         @*conn* before)]
                            ;; ALTER FUNCTION RENAME is not implemented; emulate
                            ;; its stored catalog change through the native API.
                            (d/transact *conn* [{:db/id eid
                                                 :datahike.pg.function/name after
                                                 :datahike.pg.function/key (str after "/0()")}])
                            "SELECT 1"))))

(defn- drop-object! [c kind name]
  (execute! c (case kind
                "regtype" (str "DROP TYPE " name)
                "regclass" (str "DROP TABLE " name)
                "regproc" (str "DROP FUNCTION " name "()"))))

(defn- object-oid [c kind name]
  (get-in (query c (str "SELECT to_" kind "(" (literal name) ")::oid")) [:rows 0 0]))

(defn- scalar-row [oid] {:rows [[oid]]})
(defn- array-row [oid] {:rows [[(str "{" oid "}")]]})

(defn- rename-relation-identity! [oid new-name]
  ;; SQL table rename remains explicitly unsupported. Native catalog changes
  ;; still invalidate aliases, independently of the unchanged storage names.
  (d/transact *conn*
              (objects/rename-object-tx @*conn* objects/pg-class-oid
                                        (Long/parseLong oid) new-name nil)))

(deftest type-and-routine-literals-survive-unrelated-catalog-changes
  (doseq [mode [:sql :wire] kind ["regtype" "regproc"]]
    (testing (str mode " " kind)
      (with-open [c (connect mode)]
        (execute! c "SET search_path TO public, pg_catalog")
        (let [prefix (str "poi_keep_" (name mode) "_" kind)
              object (str prefix "_object")
              renamed (str prefix "_renamed")
              other (str prefix "_other")]
          (create-object! c kind object)
          (execute! c (str "CREATE TABLE " other " (v integer)"))
          (let [old (object-oid c kind object)
                scalar (prepare c mode "poi_scalar" (str "SELECT " (literal object) "::" kind "::oid"))
                function-style (prepare c mode "poi_function_style" (str "SELECT " kind "(" (literal object) ")::oid"))
                array-literal (prepare c mode "poi_array_literal" (str "SELECT " (literal (str "{" object "}")) "::" kind "[]::oid[]"))]
            (try
              (warm! scalar (scalar-row old))
              (warm! function-style (scalar-row old))
              (warm! array-literal (array-row old))
              (rename-object! c kind object renamed)
              (execute! c (str "ALTER TABLE " other " ADD COLUMN w integer"))
              (is (= (scalar-row old) (run scalar)))
              (is (= (scalar-row old) (run function-style)))
              (is (= (array-row old) (run array-literal)))
              (drop-object! c kind renamed)
              (is (= (scalar-row old) (run scalar)))
              (is (= (array-row old) (run array-literal)))
              (create-object! c kind object)
              (let [new (object-oid c kind object)]
                (is (some? new))
                (is (not= old new))
                (is (= (scalar-row old) (run scalar)))
                (is (= (scalar-row old) (run function-style)))
                (is (= (array-row old) (run array-literal)))
                (execute! c "SET search_path TO pg_catalog, public")
                (is (= (scalar-row new) (run scalar)))
                (is (= (scalar-row new) (run function-style)))
                (is (= (array-row new) (run array-literal))))
              (finally (close! scalar) (close! function-style) (close! array-literal)))))))))

(deftest referenced-relations-and-called-functions-force-query-reanalysis
  (doseq [mode [:sql :wire] kind ["regtype" "regproc"]]
    (testing (str mode " " kind)
      (with-open [c (connect mode)]
        (execute! c "SET search_path TO public, pg_catalog")
        (let [prefix (str "poi_deps_" (name mode) "_" kind)
              object (str prefix "_object")
              table (str prefix "_table")
              called (str prefix "_called")
              missing-state (if (= kind "regtype") "42704" "42883")]
          (create-object! c kind object)
          (execute! c (str "CREATE TABLE " table " (v integer)"))
          (execute! c (str "INSERT INTO " table " VALUES (1)"))
          (execute! c (str "CREATE FUNCTION " called "() RETURNS integer LANGUAGE sql AS 'SELECT 1'"))
          (let [old (object-oid c kind object)
                expr (str (literal object) "::" kind "::oid")
                scalar (prepare c mode "poi_standalone" (str "SELECT " expr))
                from (prepare c mode "poi_from" (str "SELECT " expr " FROM " table))
                call (prepare c mode "poi_call" (str "SELECT " expr ", " called "()"))]
            (try
              (warm! scalar (scalar-row old))
              (warm! from (scalar-row old))
              (warm! call {:rows [[old "1"]]})
              (rename-object! c kind object (str object "_renamed"))
              (is (= (scalar-row old) (run scalar)))
              (is (= (scalar-row old) (run from)))
              (is (= {:rows [[old "1"]]} (run call)))
              (execute! c (str "CREATE OR REPLACE FUNCTION " called "() RETURNS integer LANGUAGE sql AS 'SELECT 2'"))
              (is (= {:sqlstate missing-state} (run call)))
              (is (= (scalar-row old) (run scalar)))
              (is (= (scalar-row old) (run from)))
              (execute! c (str "ALTER TABLE " table " ADD COLUMN w integer"))
              (is (= {:sqlstate missing-state} (run from)))
              (is (= (scalar-row old) (run scalar)))
              (execute! c "SET search_path TO pg_catalog, public")
              (is (= {:sqlstate missing-state} (run scalar)))
              (finally (close! scalar) (close! from) (close! call)))))))))

(deftest relation-constant-shape-controls-analysis-dependencies
  (doseq [mode [:sql :wire]]
    (testing (name mode)
      (with-open [c (connect mode)]
        (execute! c "SET search_path TO public, pg_catalog")
        (let [prefix (str "poi_class_" (name mode))
              table (str prefix "_table")
              type (str prefix "_type")]
          (execute! c (str "CREATE TABLE " table " (v integer)"))
          (create-object! c "regtype" type)
          (let [rel-oid (get-in (query c (str "SELECT " (literal table) "::regclass::oid")) [:rows 0 0])
                type-oid (object-oid c "regtype" type)
                class-expr (str (literal table) "::regclass")
                type-expr (str (literal type) "::regtype::oid")
                scalar (prepare c mode "poi_class_scalar" (str "SELECT " class-expr "::oid"))
                array-literal (prepare c mode "poi_class_array" (str "SELECT " (literal (str "{" table "}")) "::regclass[]::oid[]"))
                array-elements (prepare c mode "poi_class_elements" (str "SELECT ARRAY[" class-expr "]::oid[]"))
                mixed-literal (prepare c mode "poi_mixed_literal" (str "SELECT " type-expr ", " class-expr "::oid"))
                mixed-numeric (prepare c mode "poi_mixed_numeric" (str "SELECT " type-expr ", " rel-oid "::regclass::oid"))
                mixed-oid (prepare c mode "poi_mixed_oid" (str "SELECT " type-expr ", " rel-oid "::oid"))]
            (try
              (warm! scalar (scalar-row rel-oid))
              (warm! array-literal (array-row rel-oid))
              (warm! array-elements (array-row rel-oid))
              (doseq [p [mixed-literal mixed-numeric mixed-oid]]
                (warm! p {:rows [[type-oid rel-oid]]}))
              (rename-object! c "regtype" type (str type "_renamed"))
              (execute! c (str "ALTER TABLE " table " ADD COLUMN w integer"))
              (is (= {:sqlstate "42704"} (run mixed-literal)))
              (is (= {:rows [[type-oid rel-oid]]} (run mixed-numeric)))
              (is (= {:rows [[type-oid rel-oid]]} (run mixed-oid)))
              (rename-relation-identity! rel-oid (str table "_renamed"))
              (is (= {:sqlstate "42P01"} (run scalar)))
              (is (= {:sqlstate "42P01"} (run array-elements)))
              (is (= (array-row rel-oid) (run array-literal)))
              (rename-relation-identity! rel-oid table)
              (execute! c (str "DROP TABLE " table))
              (execute! c (str "CREATE TABLE " table " (v integer)"))
              (let [new (get-in (query c (str "SELECT " (literal table) "::regclass::oid")) [:rows 0 0])]
                (is (not= rel-oid new))
                (is (= (scalar-row new) (run scalar)))
                (is (= (array-row new) (run array-elements)))
                (is (= (array-row rel-oid) (run array-literal)))
                (is (= {:rows [[type-oid rel-oid]]} (run mixed-numeric)))
                (execute! c "SET search_path TO pg_catalog, public")
                (is (= (array-row new) (run array-literal)))
                (is (= {:sqlstate "42704"} (run mixed-numeric)))
                (is (= {:sqlstate "42704"} (run mixed-oid))))
              (finally
                (doseq [p [scalar array-literal array-elements mixed-literal mixed-numeric mixed-oid]]
                  (close! p))))))))))

(deftest explicit-text-conversions-and-to-reg-functions-resolve-at-execution
  (doseq [mode [:sql :wire] kind ["regtype" "regproc" "regclass"]]
    (testing (str mode " " kind)
      (with-open [c (connect mode)]
        (let [object (str "poi_runtime_" (name mode) "_" kind)
              missing-state (case kind "regtype" "42704" "regclass" "42P01" "42883")]
          (create-object! c kind object)
          (let [old (object-oid c kind object)
                text-cast (prepare c mode "poi_text_cast" (str "SELECT " (literal object) "::text::" kind "::oid"))
                text-function (prepare c mode "poi_text_function" (str "SELECT " kind "(" (literal object) "::text)::oid"))
                lookup (prepare c mode "poi_lookup" (str "SELECT to_" kind "(" (literal object) ")::oid"))]
            (try
              (doseq [p [text-cast text-function lookup]] (warm! p (scalar-row old)))
              (if (= kind "regclass")
                (drop-object! c kind object)
                (rename-object! c kind object (str object "_renamed")))
              (is (= {:sqlstate missing-state} (run text-cast)))
              (is (= {:sqlstate missing-state} (run text-function)))
              (is (= (scalar-row nil) (run lookup)))
              (create-object! c kind object)
              (let [new (object-oid c kind object)]
                (is (not= old new))
                (doseq [p [text-cast text-function lookup]]
                  (is (= (scalar-row new) (run p)))))
              (finally (close! text-cast) (close! text-function) (close! lookup)))))))))

(deftest prepared-literal-assignments-preserve-scalar-and-array-identities
  (doseq [mode [:sql :wire] kind ["regtype" "regproc"]]
    (testing (str mode " " kind)
      (with-open [c (connect mode)]
        (let [prefix (str "poi_insert_" (name mode) "_" kind)
              object (str prefix "_object")
              table (str prefix "_refs")]
          (create-object! c kind object)
          (execute! c (str "CREATE TABLE " table " (p " kind ", a " kind "[])"))
          (let [old (object-oid c kind object)
                insert (prepare c mode "poi_insert"
                                (str "INSERT INTO " table " (p,a) VALUES ("
                                     (literal object) ", " (literal (str "{" object "}")) ")"))]
            (try
              (warm! insert {:updated 1})
              (rename-object! c kind object (str object "_renamed"))
              (is (= {:updated 1} (run insert)))
              (drop-object! c kind (str object "_renamed"))
              (create-object! c kind object)
              (let [new (object-oid c kind object)]
                (is (not= old new))
                (is (= {:updated 1} (run insert)))
                (is (= {:rows (vec (repeat 8 [old (str "{" old "}")]))}
                       (query c (str "SELECT p::oid, a::oid[] FROM " table))))
                (execute! c (str "ALTER TABLE " table " ADD COLUMN w integer"))
                (is (= {:updated 1} (run insert)))
                (is (= {old 8 new 1}
                       (frequencies (map first (:rows (query c (str "SELECT p::oid FROM " table))))))))
              (finally (close! insert)))))))))

(deftest discard-plans-reanalyzes-oid-literals
  (doseq [mode [:sql :wire]]
    (with-open [c (connect mode)]
      (let [object (str "poi_discard_" (name mode))]
        (create-object! c "regtype" object)
        (let [old (object-oid c "regtype" object)
              p (prepare c mode "poi_discard" (str "SELECT " (literal object) "::regtype::oid"))]
          (try
            (warm! p (scalar-row old))
            (drop-object! c "regtype" object)
            (create-object! c "regtype" object)
            (let [new (object-oid c "regtype" object)]
              (is (not= old new))
              (is (= (scalar-row old) (run p)))
              (execute! c "DISCARD PLANS")
              (is (= (scalar-row new) (run p))))
            (finally (close! p))))))))

(deftest preparations-are-isolated-and-introspection-retains-original-sql
  (doseq [mode [:sql :wire]]
    (testing (name mode)
      (with-open [a (connect mode) b (connect mode) unnamed (connect :unnamed)]
        (let [object (str "poi_isolation_" (name mode))
              sql (str "SELECT " (literal object) "::regtype::oid")]
          (create-object! a "regtype" object)
          (let [old (object-oid a "regtype" object)
                first-plan (prepare a mode "poi_isolation" sql)]
            (try
              (warm! first-plan (scalar-row old))
              (drop-object! a "regtype" object)
              (create-object! a "regtype" object)
              (let [new (object-oid a "regtype" object)
                    second-plan (prepare b mode "poi_isolation" sql)]
                (try
                  (is (not= old new))
                  (warm! second-plan (scalar-row new))
                  (is (= (scalar-row old) (run first-plan)))
                  (is (= (scalar-row new) (query unnamed sql)))
                  (when (= mode :sql)
                    ;; The current view exposes SQL PREPARE records; wire
                    ;; statement names remain private to the protocol layer.
                    (let [statements (:rows (query a "SELECT statement FROM pg_prepared_statements"))]
                      (is (some #(str/includes? (first %) sql) statements))
                      (is (some #(str/includes? (first %) (str (literal object) "::regtype")) statements))))
                  (drop-object! a "regtype" object)
                  (create-object! a "regtype" object)
                  (let [newest (object-oid a "regtype" object)]
                    (is (not= new newest))
                    (is (= (scalar-row newest) (query unnamed sql)))
                    (is (= (scalar-row old) (run first-plan)))
                    (is (= (scalar-row new) (run second-plan))))
                  (finally (close! second-plan))))
              (finally (close! first-plan))))
          (when (= mode :sql)
            ;; DEALLOCATE removes the analyzed identity table as well as the
            ;; executable plan: the same name and SQL acquire the new OID.
            (let [current (object-oid a "regtype" object)
                  replacement (prepare a mode "poi_isolation" sql)]
              (try (is (= (scalar-row current) (run replacement)))
                   (finally (close! replacement))))))))))

(deftest temporary-shadowing-invalidates-the-analysis-environment
  (doseq [mode [:sql :wire]]
    (testing (name mode)
      (with-open [c (connect mode)]
        (execute! c "SET search_path TO public, pg_catalog")
        (let [table (str "poi_tmp_" (name mode))
              unqualified-sql (str "SELECT " (literal table) "::regclass::oid, "
                                   (literal table) "::regtype::oid")
              qualified-sql (str "SELECT " (literal (str "public." table)) "::regclass::oid, "
                                 (literal (str "public." table)) "::regtype::oid")]
          (execute! c (str "CREATE TABLE " table " (v integer)"))
          (let [expected (query c qualified-sql)
                unqualified (prepare c mode "poi_temp_unqualified" unqualified-sql)
                qualified (prepare c mode "poi_temp_qualified" qualified-sql)]
            (try
              (warm! unqualified expected)
              (warm! qualified expected)
              (execute! c (str "CREATE TEMP TABLE " table " (v integer)"))
              ;; Temporary OID aliases remain explicitly unsupported. A
              ;; retained public identity must not bypass that boundary.
              (is (= {:sqlstate "0A000"} (run unqualified)))
              (is (= expected (run qualified)))
              (execute! c (str "DROP TABLE " table))
              (is (= expected (run unqualified)))
              (is (= expected (run qualified)))
              (finally (close! unqualified) (close! qualified)))))))))

(deftest rolled-back-ddl-invalidates-only-its-analysis-dependencies
  (doseq [mode [:sql :wire] rollback-mode [:transaction :savepoint]]
    (testing (str mode " " rollback-mode)
      (with-open [c (connect mode)]
        (let [prefix (str "poi_rollback_" (name mode) "_" (name rollback-mode))
              object (str prefix "_type")
              table (str prefix "_table")
              other (str prefix "_other")
              rollback-ddl! (fn [target]
                              (.setAutoCommit c false)
                              (when (= rollback-mode :savepoint)
                                (execute! c "SAVEPOINT before_ddl"))
                              (execute! c (str "ALTER TABLE " target " ADD COLUMN w integer"))
                              (when (= rollback-mode :savepoint)
                                (execute! c "ROLLBACK TO SAVEPOINT before_ddl")
                                (execute! c "RELEASE SAVEPOINT before_ddl"))
                              (.rollback c)
                              (.setAutoCommit c true))]
          (create-object! c "regtype" object)
          (execute! c (str "CREATE TABLE " table " (v integer)"))
          (execute! c (str "INSERT INTO " table " VALUES (1)"))
          (execute! c (str "CREATE TABLE " other " (v integer)"))
          (let [old (object-oid c "regtype" object)
                p (prepare c mode "poi_rollback"
                           (str "SELECT " (literal object) "::regtype::oid FROM " table))]
            (try
              (warm! p (scalar-row old))
              (rename-object! c "regtype" object (str object "_renamed"))
              (rollback-ddl! other)
              (is (= (scalar-row old) (run p)))
              (is (= {:sqlstate "42703"} (query c (str "SELECT w FROM " other))))
              ;; The restored DB equals its earlier definition, but the
              ;; relation invalidation event still requires fresh analysis.
              (rollback-ddl! table)
              (is (= {:sqlstate "42703"} (query c (str "SELECT w FROM " table))))
              (is (= {:sqlstate "42704"} (run p)))
              (finally (close! p)))))))))

(deftest literal-sites-and-bound-parameters-keep-separate-meanings
  (doseq [mode [:sql :wire]]
    (testing (name mode)
      (with-open [c (connect mode)]
        (execute! c "SET search_path TO public, pg_catalog")
        (let [object (str "poi_mixed_sites_" (name mode))
              sql (str "SELECT " (literal object) "::regtype::oid, "
                       (literal object) "::text, to_regtype(" (literal object) ")::oid, "
                       (if (= mode :sql) "$1" "?") "::int")]
          (create-object! c "regtype" object)
          (let [old (object-oid c "regtype" object)
                p (if (= mode :sql)
                    (do (execute! c (str "PREPARE poi_mixed_sites AS " sql))
                        {:run-with #(query c (str "EXECUTE poi_mixed_sites(" % ")"))
                         :close #(execute! c "DEALLOCATE poi_mixed_sites")})
                    (let [st (.prepareStatement c sql)]
                      (.setPrepareThreshold ^PGStatement st 5)
                      {:run-with (fn [n] (.setInt st 1 (int n)) (execute-statement st nil))
                       :close #(.close st)}))
                run-with (:run-with p)]
            (try
              (doseq [n (range 1 7)]
                (is (= {:rows [[old object old (str n)]]} (run-with n))))
              (rename-object! c "regtype" object (str object "_renamed"))
              (is (= {:rows [[old object nil "7"]]} (run-with 7)))
              (drop-object! c "regtype" (str object "_renamed"))
              (create-object! c "regtype" object)
              (let [new (object-oid c "regtype" object)]
                (is (not= old new))
                (is (= {:rows [[old object new "8"]]} (run-with 8)))
                (execute! c "SET search_path TO pg_catalog, public")
                (is (= {:rows [[new object new "9"]]} (run-with 9))))
              (finally (close! p)))))))))

(deftest implicit-predicate-literals-retain-their-analyzed-identities
  (doseq [mode [:sql :wire]]
    (with-open [c (connect mode)]
      (let [object (str "poi_pred_type_" (name mode))
            table (str "poi_pred_refs_" (name mode))]
        (create-object! c "regtype" object)
        (execute! c (str "CREATE TABLE " table " (p regtype)"))
        (execute! c (str "INSERT INTO " table " VALUES (" (literal object) ")"))
        (let [old (object-oid c "regtype" object)
              sql (str "SELECT " (literal object) "::regtype::oid FROM " table
                       " WHERE p IN (" (literal object) ") AND p BETWEEN "
                       (literal object) " AND " (literal object))
              p (prepare c mode "poi_predicates" sql)]
          (try
            (warm! p (scalar-row old))
            (rename-object! c "regtype" object (str object "_renamed"))
            (is (= (scalar-row old) (run p)))
            (create-object! c "regtype" object)
            (is (not= old (object-oid c "regtype" object)))
            (is (= (scalar-row old) (run p)))
            (is (= {:rows []} (query c sql)))
            (finally (close! p))))))))

(deftest a-cte-name-does-not-hide-a-qualified-relation-dependency
  (doseq [mode [:sql :wire]]
    (with-open [c (connect mode)]
      (let [object (str "poi_cte_type_" (name mode))
            table (str "poi_cte_table_" (name mode))]
        (create-object! c "regtype" object)
        (execute! c (str "CREATE TABLE " table " (v integer)"))
        (execute! c (str "INSERT INTO " table " VALUES (1)"))
        (let [old (object-oid c "regtype" object)
              p (prepare c mode "poi_cte_dependency"
                         (str "WITH " table " AS (SELECT 2 AS v) SELECT "
                              (literal object) "::regtype::oid FROM public." table))]
          (try
            (warm! p (scalar-row old))
            (rename-object! c "regtype" object (str object "_renamed"))
            (is (= (scalar-row old) (run p)))
            (execute! c (str "ALTER TABLE " table " ADD COLUMN w integer"))
            (is (= {:sqlstate "42704"} (run p)))
            (finally (close! p))))))))

(deftest custom-datatypes-decline-the-whole-analysis
  (with-open [c (connect :sql)]
    (create-object! c "regtype" "poi_guard_type")
    (execute! c "CREATE TABLE poi_guard_refs (x poi_guard_type, v integer)")
    (execute! c "CREATE TABLE poi_guard_plain (v integer)")
    (let [h (pg/make-query-handler *conn*)
          analyze #(.parse h ^String % (int-array 0))]
      (doseq [sql ["SELECT 'poi_guard_type'::regtype::oid, 'a'::poi_guard_type"
                   "SELECT 'poi_guard_type'::regtype::oid, x FROM poi_guard_refs"
                   "SELECT 'poi_guard_type'::regtype::oid, v FROM poi_guard_refs"]]
        (testing sql
          (let [p (analyze sql)]
            (is (not= :error (:type p)))
            (is (nil? (:prepared-analysis p))))))
      ;; Naming a user type as an alias value is supported; using it as
      ;; an actual result/column datatype needs a broader analyzed tree.
      (is (some? (:prepared-analysis
                  (analyze "SELECT 'poi_guard_type'::regtype::oid FROM poi_guard_plain")))))))

(deftest bound-parameter-reparsing-uses-the-session-search-path
  (with-open [c (connect :wire)]
    (create-object! c "regtype" "poi_hidden")
    (execute! c "SET search_path TO pg_catalog")
    (with-open [p (.prepareStatement
                   c (str "SELECT generate_series(1, CASE WHEN to_regtype('poi_hidden') "
                          "IS NULL THEN ?::int ELSE 0 END)"))]
      (.setPrepareThreshold ^PGStatement p 5)
      (doseq [n [2 3 1 2 3 1 2]]
        (.setInt p 1 n)
        (is (= {:rows (mapv #(vector (str %)) (range 1 (inc n)))}
               (execute-statement p nil))))
      (execute! c "SET search_path TO public, pg_catalog")
      (is (= {:rows []} (execute-statement p nil)))
      (execute! c "SET search_path TO pg_catalog")
      (is (= {:rows [["1"] ["2"]]} (execute-statement p nil))))))

(deftest builtin-and-stored-function-collisions-decline-analysis
  (with-open [c (connect :sql)]
    (create-object! c "regtype" "poi_builtin_collision")
    (execute! c "CREATE FUNCTION now() RETURNS integer LANGUAGE sql AS 'SELECT 2'")
    (let [h (pg/make-query-handler *conn*)
          p (.parse h "SELECT 'poi_builtin_collision'::regtype::oid, now()" (int-array 0))]
      (is (not= :error (:type p)))
      (is (nil? (:prepared-analysis p))))))

(deftest sequence-position-is-not-an-analysis-dependency
  (doseq [mode [:sql :wire]]
    (with-open [c (connect mode)]
      (let [object (str "poi_seq_type_" (name mode))
            sequence (str "poi_seq_" (name mode))
            other (str "poi_seq_other_" (name mode))]
        (create-object! c "regtype" object)
        (execute! c (str "CREATE SEQUENCE " sequence))
        (execute! c (str "CREATE TABLE " other " (v integer)"))
        (let [old (object-oid c "regtype" object)
              p (prepare c mode "poi_seq_plan"
                         (str "SELECT " (literal object) "::regtype::oid, nextval("
                              (literal sequence) ")::bigint"))]
          (try
            (doseq [n (range 1 7)]
              (is (= {:rows [[old (str n)]]} (run p))))
            (rename-object! c "regtype" object (str object "_renamed"))
            (is (= {:rows [[old "7"]]} (run p)))
            (execute! c (str "ALTER TABLE " other " ADD COLUMN w integer"))
            (is (= {:rows [[old "8"]]} (run p)))
            (execute! c "DISCARD PLANS")
            (is (= {:sqlstate "42704"} (run p)))
            (finally (close! p))))))))

(defn- rewrite-and-optionally-rollback! [^Connection c sql rollback?]
  (if rollback?
    (do (.setAutoCommit c false)
        (try (execute! c sql)
             (finally (.rollback c) (.setAutoCommit c true))))
    (execute! c sql)))

(deftest sequence-storage-rewrites-invalidate-analysis
  (doseq [mode [:sql :wire]
          [label options] [["restart" "RESTART WITH 10"]
                           ["increment" "INCREMENT BY 1"]
                           ["cache" "CACHE 1"]]
          rollback? [false true]]
    (testing (str mode " " options " rollback=" rollback?)
      (with-open [c (connect mode)]
        (let [prefix (str "poi_srw_" (name mode) "_" label "_" rollback?)
              object (str prefix "_type")
              sequence (str prefix "_seq")]
          (create-object! c "regtype" object)
          (execute! c (str "CREATE SEQUENCE " sequence))
          (let [old (object-oid c "regtype" object)
                p (prepare c mode "poi_sequence_rewrite"
                           (str "SELECT " (literal object) "::regtype::oid, nextval("
                                (literal sequence) ")::bigint"))]
            (try
              (doseq [n (range 1 7)]
                (is (= {:rows [[old (str n)]]} (run p))))
              (rename-object! c "regtype" object (str object "_renamed"))
              (is (= {:rows [[old "7"]]} (run p)))
              (rewrite-and-optionally-rollback!
               c (str "ALTER SEQUENCE " sequence " " options) rollback?)
              (is (= {:sqlstate "42704"} (run p)))
              (is (= {:rows [[(if (and (= label "restart") (not rollback?)) "10" "8")]]}
                     (query c (str "SELECT nextval(" (literal sequence) ")"))))
              (finally (close! p)))))))))

(deftest truncate-invalidates-only-referenced-relations
  (doseq [mode [:sql :wire] rollback? [false true]]
    (with-open [c (connect mode)]
      (let [prefix (str "poi_tr_" (name mode) "_" rollback?)
            object (str prefix "_type")
            table (str prefix "_table")
            other (str prefix "_other")]
        (create-object! c "regtype" object)
        (execute! c (str "CREATE TABLE " table " (v integer)"))
        (execute! c (str "INSERT INTO " table " VALUES (1)"))
        (execute! c (str "CREATE TABLE " other " (v integer)"))
        (let [old (object-oid c "regtype" object)
              p (prepare c mode "poi_truncate"
                         (str "SELECT " (literal object) "::regtype::oid FROM " table))]
          (try
            (warm! p (scalar-row old))
            (rename-object! c "regtype" object (str object "_renamed"))
            (execute! c (str "TRUNCATE " other))
            (is (= (scalar-row old) (run p)))
            (rewrite-and-optionally-rollback! c (str "TRUNCATE " table) rollback?)
            (is (= {:sqlstate "42704"} (run p)))
            (is (= {:rows [[(if rollback? "1" "0")]]}
                   (query c (str "SELECT count(*) FROM " table))))
            (finally (close! p))))))))

(deftest truncate-identity-storage-invalidates-sequence-dependents
  (doseq [mode [:sql :wire] rollback? [false true]]
    (with-open [c (connect mode)]
      (let [prefix (str "poi_tri_" (name mode) "_" rollback?)
            object (str prefix "_type")
            table (str prefix "_table")
            sequence (str table "_id_seq")]
        (create-object! c "regtype" object)
        (execute! c (str "CREATE TABLE " table " (id integer GENERATED ALWAYS AS IDENTITY)"))
        (let [old (object-oid c "regtype" object)
              p (prepare c mode "poi_truncate_identity"
                         (str "SELECT " (literal object) "::regtype::oid, nextval("
                              (literal sequence) ")::bigint"))]
          (try
            (doseq [n (range 1 7)]
              (is (= {:rows [[old (str n)]]} (run p))))
            (rename-object! c "regtype" object (str object "_renamed"))
            (is (= {:rows [[old "7"]]} (run p)))
            (rewrite-and-optionally-rollback!
             c (str "TRUNCATE " table " RESTART IDENTITY") rollback?)
            (is (= {:sqlstate "42704"} (run p)))
            (is (= {:rows [[(if rollback? "8" "1")]]}
                   (query c (str "SELECT nextval(" (literal sequence) ")"))))
            (finally (close! p))))))))

(deftest storage-revisions-address-objects-created-in-the-same-transaction
  (doseq [mode [:sql :wire]]
    (with-open [c (connect mode)]
      (let [table (str "poi_new_storage_" (name mode))
            sequence (str "poi_new_sequence_" (name mode))]
        (.setAutoCommit c false)
        (execute! c (str "CREATE SEQUENCE " sequence))
        (execute! c (str "ALTER SEQUENCE " sequence " RESTART WITH 5"))
        (execute! c (str "CREATE TABLE " table " (id integer GENERATED ALWAYS AS IDENTITY)"))
        (execute! c (str "INSERT INTO " table " DEFAULT VALUES"))
        (execute! c (str "TRUNCATE " table " RESTART IDENTITY"))
        (execute! c (str "INSERT INTO " table " DEFAULT VALUES"))
        (.commit c)
        (.setAutoCommit c true)
        (is (= {:rows [["1"]]} (query c (str "SELECT id FROM " table))))
        (is (= {:rows [["5"]]} (query c (str "SELECT nextval(" (literal sequence) ")"))))
        (is (= {:rows [["2"]]}
               (query c (str "SELECT nextval(" (literal (str table "_id_seq")) ")"))))
        ;; A sequence rewrite and its reservations disappear together when
        ;; a savepoint restores the pre-rewrite storage generation.
        (.setAutoCommit c false)
        (execute! c "SAVEPOINT before_restart")
        (execute! c (str "ALTER SEQUENCE " sequence " RESTART WITH 20"))
        (is (= {:rows [["20"]]} (query c (str "SELECT nextval(" (literal sequence) ")"))))
        (execute! c "ROLLBACK TO SAVEPOINT before_restart")
        (is (= {:rows [["6"]]} (query c (str "SELECT nextval(" (literal sequence) ")"))))
        (.commit c)
        (.setAutoCommit c true)
        (is (= {:rows [["7"]]} (query c (str "SELECT nextval(" (literal sequence) ")"))))))))

(deftest selecting-an-identity-table-does-not-read-its-owned-sequence-definition
  (doseq [mode [:sql :wire]]
    (with-open [c (connect mode)]
      (let [object (str "poi_unread_type_" (name mode))
            table (str "poi_unread_owner_" (name mode))]
        (create-object! c "regtype" object)
        (execute! c (str "CREATE TABLE " table " (id integer GENERATED ALWAYS AS IDENTITY)"))
        (execute! c (str "INSERT INTO " table " DEFAULT VALUES"))
        (let [old (object-oid c "regtype" object)
              p (prepare c mode "poi_unread_sequence"
                         (str "SELECT " (literal object) "::regtype::oid FROM " table))]
          (try
            (warm! p (scalar-row old))
            (rename-object! c "regtype" object (str object "_renamed"))
            (is (= (scalar-row old) (run p)))
            (execute! c (str "ALTER SEQUENCE " table "_id_seq INCREMENT BY 2"))
            (is (= (scalar-row old) (run p)))
            (finally (close! p))))))))

(deftest implicit-write-expressions-decline-partial-analysis
  (with-open [c (connect :sql)]
    (execute! c "CREATE TABLE poi_guard_default (p regtype, v integer DEFAULT 1)")
    (execute! c "CREATE TABLE poi_guard_identity (p regtype, id integer GENERATED ALWAYS AS IDENTITY)")
    (execute! c (str "CREATE TABLE poi_guard_generated (p regtype, v integer, "
                     "g integer GENERATED ALWAYS AS (v + 1) STORED)"))
    (let [h (pg/make-query-handler *conn*)
          analyze #(.parse h ^String % (int-array 0))]
      (doseq [table ["poi_guard_default" "poi_guard_identity" "poi_guard_generated"]]
        (doseq [sql [(str "INSERT INTO " table " (p) VALUES ('int4')")
                     (str "UPDATE " table " SET p = 'int4'")]]
          (testing sql
            (let [p (analyze sql)]
              (is (not= :error (:type p)))
              (is (nil? (:prepared-analysis p))))))
        (is (some? (:prepared-analysis
                    (analyze (str "SELECT 'int4'::regtype::oid FROM " table)))))))))

(deftest identity-sequence-registration-preserves-existing-relation-identities
  (with-open [c (connect :sql)]
    (execute! c "CREATE TABLE poi_catalog_owner_id_seq (v integer)")
    (let [old (object-oid c "regclass" "poi_catalog_owner_id_seq")]
      (is (= {:sqlstate "42P07"}
             (query c "CREATE TABLE poi_catalog_owner (id integer GENERATED ALWAYS AS IDENTITY)")))
      (is (= old (object-oid c "regclass" "poi_catalog_owner_id_seq")))
      (is (= {:rows [["r"]]}
             (query c (str "SELECT relkind FROM pg_class WHERE oid = " old))))
      (is (= {:rows [[nil]]}
             (query c "SELECT to_regclass('poi_catalog_owner')::oid"))))))

(deftest owned-sequence-catalog-identities-follow-table-lifecycle
  (doseq [mode [:sql :wire]
          [label declaration] [["identity" "integer GENERATED ALWAYS AS IDENTITY"]
                               ["serial" "serial"]]]
    (with-open [c (connect mode)]
      (let [table (str "poi_owned_" (name mode) "_" label)
            sequence (str table "_id_seq")
            create-sql (str "CREATE TABLE " table " (id " declaration ")")]
        (execute! c create-sql)
        (let [old (object-oid c "regclass" sequence)]
          (is (some? old))
          (.setAutoCommit c false)
          (execute! c (str "DROP TABLE " table))
          (is (nil? (object-oid c "regclass" sequence)))
          (.rollback c)
          (.setAutoCommit c true)
          (is (= old (object-oid c "regclass" sequence)))
          (execute! c (str "DROP TABLE " table))
          (is (nil? (object-oid c "regclass" sequence)))
          (is (= {:rows [["0"]]}
                 (query c (str "SELECT count(*) FROM pg_class WHERE oid = " old))))
          (execute! c create-sql)
          (let [new (object-oid c "regclass" sequence)]
            (is (some? new))
            (is (not= old new))
            (is (= {:rows [["S"]]}
                   (query c (str "SELECT relkind FROM pg_class WHERE oid = " new))))))))))
