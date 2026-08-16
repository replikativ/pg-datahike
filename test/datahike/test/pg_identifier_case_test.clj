(ns datahike.test.pg-identifier-case-test
  "PostgreSQL folds UNQUOTED identifiers to lower case; quoted ones keep
   their case. We stored whatever was typed, so a table created as
   `MixedCase` was unreachable as `mixedcase` (42P01), a column of a
   lower-cased table was unreachable as `ColA` (it read as NULL), and —
   the damaging one — `pg_class.relname` reported `MixedCase`. A client
   that folds the name the way PostgreSQL does and then reflects it found
   nothing, which breaks ORM schema reflection and pg_dump for any
   mixed-case DDL.

   The fold happens in `params/unquote-ident`, because the
   quoted/unquoted distinction lives only in the raw text JSqlParser
   hands us and is gone the moment the quotes are stripped.

   Storage is NOT migrated, and that is the point of the second half of
   this file. Two populations keep their case forever and must stay
   reachable through a folded reference:

     - a database created before this change, holding `:MixedCase/ColA`;
     - a Datalog-native database, holding e.g. `:person/firstName` —
       serving one of those is a headline feature.

   A case-folding index over the schema resolves a folded reference back
   to the stored name, exact match first. The WRITE paths go through it
   too: an INSERT that folded without resolving would assert
   `:mixedcase/cola` beside an existing `:MixedCase/ColA` and split the
   table in two, silently.

   Expectations captured from PostgreSQL 17."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.params :as params])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)
(def ^:dynamic *conn* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*conn* conn
                  *handler* (pg/make-query-handler conn)]
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- run [sql] (.execute *handler* sql))
(defn- rows [sql] (mapv vec (.-rows ^PgWireServer$QueryResult (run sql))))
(defn- v [sql] (ffirst (rows sql)))
(defn- err [sql]
  (try (.-error ^PgWireServer$QueryResult (run sql))
       (catch Exception e (ex-message e))))

;; ---------------------------------------------------------------------------
;; The lexical rule
;; ---------------------------------------------------------------------------

(deftest fold-identifier-unit
  (testing "unquoted folds"
    (is (= "foo" (params/unquote-ident "Foo")))
    (is (= "foo" (params/unquote-ident "FOO"))))
  (testing "quoted keeps its case and un-escapes doubled quotes"
    (is (= "Foo" (params/unquote-ident "\"Foo\"")))
    (is (= "a\"b" (params/unquote-ident "\"a\"\"b\""))))
  (testing "truncation at NAMEDATALEN-1"
    (is (= 63 (count (params/unquote-ident (apply str (repeat 70 "x")))))))
  (testing "ASCII-only, like PostgreSQL under standard encodings"
    (is (= "Ä" (params/unquote-ident "Ä"))))
  (testing "locale-independent — clojure.string/lower-case would fold
            \"ID\" to \"ıd\" under a Turkish default locale"
    (let [before (java.util.Locale/getDefault)]
      (try
        (java.util.Locale/setDefault (java.util.Locale. "tr" "TR"))
        (is (= "id" (params/unquote-ident "ID")))
        (finally (java.util.Locale/setDefault before))))))

;; ---------------------------------------------------------------------------
;; DDL storage and reference resolution
;; ---------------------------------------------------------------------------

(deftest create-table-folds-unquoted-names
  (run "CREATE TABLE MixedCase (ColA int PRIMARY KEY, ColB text)")
  (let [schema (:schema (d/db *conn*))
        ks (filter keyword? (keys schema))]
    (is (some #(= :mixedcase/cola %) ks))
    (is (some #(= :mixedcase/colb %) ks))
    (is (not-any? #(= "MixedCase" (namespace %)) ks))))

(deftest create-table-quoted-keeps-case
  (run "CREATE TABLE \"MixedCase\" (\"ColA\" int PRIMARY KEY)")
  (let [ks (filter keyword? (keys (:schema (d/db *conn*))))]
    (is (some #(= :MixedCase/ColA %) ks))))

(deftest references-fold-to-reach-a-folded-table
  (run "CREATE TABLE MixedCase (ColA int PRIMARY KEY, ColB text)")
  (run "INSERT INTO MixedCase VALUES (1, 'v')")
  (testing "every spelling reaches the same relation"
    (is (= "1" (v "SELECT ColA FROM MixedCase")))
    (is (= "1" (v "SELECT cola FROM mixedcase")))
    (is (= "1" (v "SELECT COLA FROM MIXEDCASE")))
    (is (= "v" (v "SELECT ColB FROM mixedcase")))
    (is (= "1" (v "SELECT m.cola FROM mixedcase m"))))
  (testing "SELECT * and a mixed-case reference to a lower-cased table"
    (is (= [["1" "v"]] (rows "SELECT * FROM mixedcase")))))

(deftest mixed-case-reference-to-a-lowercase-table
  (run "CREATE TABLE lower_t (cola int PRIMARY KEY)")
  (run "INSERT INTO lower_t VALUES (5)")
  (is (= "5" (v "SELECT ColA FROM lower_t")) "used to read as NULL")
  (is (= "5" (v "SELECT COLA FROM LOWER_T"))))

(deftest catalog-reflection-reports-the-folded-name
  (testing "the ORM-breaking case: create as MixedCase, reflect as mixedcase"
    (run "CREATE TABLE MixedCase (ColA int PRIMARY KEY)")
    (is (= [["mixedcase"]]
           (rows "SELECT relname FROM pg_class WHERE relname = 'mixedcase'")))
    (is (= [["mixedcase"]]
           (rows (str "SELECT table_name FROM information_schema.tables "
                      "WHERE table_name = 'mixedcase'"))))))

(deftest create-table-does-not-mint-a-case-twin
  (run "CREATE TABLE MixedCase (ColA int PRIMARY KEY)")
  (is (re-find #"already exists" (or (err "CREATE TABLE mixedcase (x int)") "")))
  (is (re-find #"already exists" (or (err "CREATE TABLE MixedCase (x int)") ""))))

;; ---------------------------------------------------------------------------
;; Compatibility: storage that predates the fold, and Datalog-native names
;; ---------------------------------------------------------------------------

(defn- seed-legacy! []
  (d/transact *conn* [{:db/ident :MixedCase/ColA :db/valueType :db.type/long
                       :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                      {:db/ident :MixedCase/ColB :db/valueType :db.type/string
                       :db/cardinality :db.cardinality/one}])
  (d/transact *conn* [{:MixedCase/ColA 1 :MixedCase/ColB "legacy"}]))

(deftest a-database-created-before-folding-stays-readable
  (seed-legacy!)
  (is (= [["1" "legacy"]] (rows "SELECT ColA, ColB FROM MixedCase")))
  (is (= [["1" "legacy"]] (rows "SELECT cola, colb FROM mixedcase"))
      "the folded reference must reach the stored name"))

(deftest writes-land-on-the-stored-attributes-not-a-folded-twin
  (testing "the silent table-split hazard"
    (seed-legacy!)
    (run "INSERT INTO mixedcase (cola, colb) VALUES (2, 'new')")
    (is (= #{["1" "legacy"] ["2" "new"]} (set (rows "SELECT ColA, ColB FROM MixedCase")))
        "both rows visible through the stored spelling")
    (is (= #{["1" "legacy"] ["2" "new"]} (set (rows "SELECT cola, colb FROM mixedcase")))
        "and through the folded one")
    (let [ks (filter keyword? (keys (:schema (d/db *conn*))))]
      (is (not-any? #(= "mixedcase" (namespace %)) ks)
          "no folded twin namespace was created"))))

(deftest datalog-native-camelcase-attributes-are-reachable
  (d/transact *conn* [{:db/ident :person/id :db/valueType :db.type/long
                       :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                      {:db/ident :person/firstName :db/valueType :db.type/string
                       :db/cardinality :db.cardinality/one}])
  (d/transact *conn* [{:person/id 1 :person/firstName "Alice"}])
  (testing "serving an existing Datahike database is a headline feature"
    (is (= "Alice" (v "SELECT firstName FROM person")))
    (is (= "Alice" (v "SELECT firstname FROM person")))
    (is (= "Alice" (v "SELECT FIRSTNAME FROM person"))))
  (testing "and in predicates"
    (is (= [["1"]] (rows "SELECT id FROM person WHERE firstName = 'Alice'")))
    (is (= [["1"]] (rows "SELECT id FROM person WHERE firstname = 'Alice'")))))

;; ---------------------------------------------------------------------------
;; Column labels
;; ---------------------------------------------------------------------------

(deftest output-column-labels-fold
  (run "CREATE TABLE lc (a int)")
  (is (= ["foo"] (vec (.-columnNames ^PgWireServer$QueryResult (run "SELECT 1 AS Foo")))))
  (is (= ["Foo"] (vec (.-columnNames ^PgWireServer$QueryResult (run "SELECT 1 AS \"Foo\"")))))
  (testing "a reserved word aliased unquoted still folds — the rewrite
            adds quotes only to get it past the parser"
    (is (= ["select"]
           (vec (.-columnNames ^PgWireServer$QueryResult (run "SELECT 1 AS Select")))))))
