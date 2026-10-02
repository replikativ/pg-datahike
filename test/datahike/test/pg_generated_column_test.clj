(ns datahike.test.pg-generated-column-test
  "`GENERATED ALWAYS AS (expr) STORED`.

   It used to be an explicit boundary -- `stored generated column \"b\"
   is not supported` -- and that was the right refusal while the value
   could not be computed: silently creating an ordinary writable column
   also lets a later COPY name it, at which point psql enters COPY mode
   and consumes the following SQL as row data.

   It is not a DEFAULT, which is why it does not reuse the
   `:pg/default-*` machinery. A default fills a column that was OMITTED;
   this replaces whatever was written, and its expression may name the
   row's other columns, which a default may not.

   VIRTUAL (PostgreSQL 18) keeps the refusal: the two differ in when the
   expression runs, and silently picking STORED would make a column that
   disagrees with the statement that created it.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
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
          {:keys [server]} (pg/start-server {"gen" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port* "/gen?user=x&password=x&sslmode=disable")))

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

(defn- state-of [^Connection c sql]
  (try (exec! c sql) nil (catch SQLException e (.getSQLState e))))

(defn- message-of [^Connection c sql]
  (try (exec! c sql) nil (catch SQLException e (.getMessage e))))

(deftest the-expression-is-computed-on-insert
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE gt (a int, b int GENERATED ALWAYS AS (a*2) STORED)")
    (exec! c "INSERT INTO gt (a) VALUES (5)")
    (is (= [["5" "10"]] (rows c "SELECT a,b FROM gt")))
    (testing "a constant expression, and one over another type"
      (exec! c "CREATE TABLE gk (a int, b int GENERATED ALWAYS AS (55) STORED)")
      (exec! c "INSERT INTO gk (a) VALUES (1)")
      (is (= [["1" "55"]] (rows c "SELECT a,b FROM gk")))
      (exec! c (str "CREATE TABLE gc (a int, b text GENERATED ALWAYS AS "
                    "(upper(a::text) || '!') STORED)"))
      (exec! c "INSERT INTO gc (a) VALUES (42)")
      (is (= [["42" "42!"]] (rows c "SELECT a,b FROM gc"))))))

(deftest a-written-value-is-refused-but-DEFAULT-is-not
  ;; PostgreSQL refuses rather than silently overwriting, because a
  ;; caller who wrote a value believes it will be stored. DEFAULT is the
  ;; one accepted spelling, and it reaches the write path as an OMITTED
  ;; column, so its absence is what permits it.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE gt (a int, b int GENERATED ALWAYS AS (a*2) STORED)")
    (is (= "428C9" (state-of c "INSERT INTO gt (a,b) VALUES (1,2)")))
    (is (re-find #"cannot insert a non-DEFAULT value into column \"b\""
                 (message-of c "INSERT INTO gt (a,b) VALUES (1,2)")))
    (exec! c "INSERT INTO gt VALUES (7, DEFAULT)")
    (is (= [["7" "14"]] (rows c "SELECT a,b FROM gt")))))

(deftest update-recomputes-and-refuses-assignment
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE gt (a int, b int GENERATED ALWAYS AS (a*2) STORED)")
    (exec! c "INSERT INTO gt (a) VALUES (5)")
    (testing "updating a column the expression names changes it, even
              though the statement never mentioned it"
      (exec! c "UPDATE gt SET a = 11")
      (is (= [["11" "22"]] (rows c "SELECT a,b FROM gt"))))
    (testing "assigning to it is 428C9, and DEFAULT is allowed"
      (is (= "428C9" (state-of c "UPDATE gt SET b = 99")))
      (is (re-find #"column \"b\" can only be updated to DEFAULT"
                   (message-of c "UPDATE gt SET b = 99")))
      (exec! c "UPDATE gt SET b = DEFAULT")
      (is (= [["11" "22"]] (rows c "SELECT a,b FROM gt"))))
    (testing "SET a = NULL takes b with it -- the RETRACT has to leave
              the scope, or the expression recomputes from the OLD value
              and b keeps its previous number"
      (exec! c "UPDATE gt SET a = NULL")
      (is (= [[nil nil]] (rows c "SELECT a,b FROM gt"))))))

(deftest copy-cannot-name-one
  ;; 42P10, not the 428C9 INSERT and UPDATE raise -- PostgreSQL's own
  ;; distinction. Checked before the connection enters COPY mode,
  ;; because otherwise psql consumes the following SQL as row data.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE gt (a int, b int GENERATED ALWAYS AS (a*2) STORED)")
    (is (= "42P10" (state-of c "COPY gt (a,b) FROM STDIN")))
    (is (re-find #"column \"b\" is a generated column"
                 (message-of c "COPY gt (a,b) FROM STDIN")))
    (testing "and the connection is still usable afterwards"
      (exec! c "INSERT INTO gt (a) VALUES (3)")
      (is (= [["3" "6"]] (rows c "SELECT a,b FROM gt"))))))

(deftest the-catalog-says-which-columns-are-generated
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE gt (a int, b int GENERATED ALWAYS AS (a*2) STORED)")
    (is (= [["a" ""] ["b" "s"]]
           (rows c (str "SELECT attname, attgenerated FROM pg_attribute "
                        "WHERE attrelid = 'gt'::regclass AND attnum > 0 "
                        "ORDER BY attnum"))))))

(deftest virtual-is-still-refused
  ;; PostgreSQL 18's. Refusing is loud; treating it as STORED would be a
  ;; column that disagrees with the statement that made it.
  (with-open [c (jdbc)]
    (is (re-find #"virtual generated column \"b\" is not supported"
                 (message-of c (str "CREATE TABLE gv (a int, b int "
                                    "GENERATED ALWAYS AS (a+1) VIRTUAL)"))))))

(deftest the-catalog-carries-the-generation-clause
  ;; `pg_attrdef` was empty and `atthasdef` false for a generated
  ;; column, which is where `pg_dump` reads the generation clause from
  ;; -- so a dump emitted the column as an ordinary one and silently
  ;; lost GENERATED ALWAYS AS. `information_schema.columns.is_generated`
  ;; was hardcoded "NEVER", which is the view every portable tool reads
  ;; instead of pg_attribute.
  (with-open [c (jdbc)]
    (exec! c (str "CREATE TABLE kt (id int PRIMARY KEY, a int, "
                  "g int GENERATED ALWAYS AS (a*2) STORED, d int DEFAULT 7)"))
    (testing "pg_attribute.attgenerated was already right; atthasdef was not"
      (is (= [["id" "" "f"] ["a" "" "f"] ["g" "s" "t"] ["d" "" "t"]]
             (rows c (str "SELECT attname, attgenerated, atthasdef FROM pg_attribute "
                          "WHERE attrelid = 'kt'::regclass AND attnum > 0 "
                          "ORDER BY attnum")))))
    (testing "the expression is in pg_attrdef, parenthesised as pg_get_expr prints it"
      (is (= [["g" "(a * 2)"] ["d" "7"]]
             (rows c (str "SELECT a.attname, pg_get_expr(d.adbin, d.adrelid) "
                          "FROM pg_attrdef d JOIN pg_attribute a "
                          "ON a.attrelid = d.adrelid AND a.attnum = d.adnum "
                          "WHERE d.adrelid = 'kt'::regclass ORDER BY a.attnum")))))
    (testing "and information_schema agrees"
      (is (= [["g" "ALWAYS" "(a * 2)"]]
             (rows c (str "SELECT column_name, is_generated, generation_expression "
                          "FROM information_schema.columns "
                          "WHERE table_name = 'kt' AND is_generated = 'ALWAYS'"))))
      (is (= [["NEVER"]]
             (rows c (str "SELECT is_generated FROM information_schema.columns "
                          "WHERE table_name = 'kt' AND column_name = 'a'")))))))
