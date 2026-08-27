(ns datahike.test.pg-tsearch-test
  "PostgreSQL 17 full-text semantic slices. The source line references in the
   campaign point to unmodified REL_17_7 tsearch.sql."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.arrays :as pg-arr]
            [datahike.pg.server :as pg]
            [datahike.pg.tsearch :as tsearch]
            [datahike.pg.types :as types])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*handler* (pg/make-query-handler conn)] (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- result [sql] (.execute *handler* sql))

(defn- rows [sql]
  (mapv vec (.-rows ^PgWireServer$QueryResult (result sql))))

(defn- state [sql]
  (try
    (.-sqlstate ^PgWireServer$QueryResult (result sql))
    (catch Exception e (:sqlstate (ex-data e)))))

(deftest postgres-tsearch-dictionary-slice
  (testing "REL_17_7 tsearch.sql lines 274-275"
    (is (= "{sky}" (pg-arr/to-pg-text (tsearch/ts-lexize "english_stem" "skies"))))
    (is (= "{ident}" (pg-arr/to-pg-text (tsearch/ts-lexize "english_stem" "identity")))))
  (testing "dictionary results retain PostgreSQL's text[] wire type"
    (let [r (result "SELECT ts_lexize('english_stem', 'skies')")]
      (is (= [["{sky}"]] (mapv vec (.-rows ^PgWireServer$QueryResult r))))
      (is (= [types/oid-text-array] (vec (.-columnOids ^PgWireServer$QueryResult r)))))))

(deftest dictionary-and-configuration-names-are-distinct
  (testing "a configuration is not a dictionary, nor vice versa"
    (is (= "42704" (state "SELECT ts_lexize('english', 'skies')")))
    (is (= "42704" (state "SELECT plainto_tsquery('english_stem', 'skies')"))))
  (testing "unknown catalog names fail instead of silently using simple"
    (is (= "42704" (state "SELECT ts_lexize('no_such_dictionary', 'skies')")))
    (is (= "42704" (state "SELECT phraseto_tsquery('no_such_config', 'skies')")))))

(deftest postgres-plain-query-slice
  (testing "REL_17_7 tsearch.sql line 315"
    (is (= "'z' & '1' & 'fghj'"
           (tsearch/plainto-tsquery "english" "the and z 1))& fghj")))
    (is (= [["'z' & '1' & 'fghj'"]]
           (rows "SELECT plainto_tsquery('english', 'the and z 1))& fghj')")))))

(deftest postgres-phrase-query-slice
  (testing "REL_17_7 tsearch.sql line 367; stop words affect distance"
    (let [expected "'postgresql' <3> 'extend' <3> 'user' <2> 'mani' <-> 'way'"
          sql (str "SELECT phraseto_tsquery('english', "
                   "'PostgreSQL can be extended by the user in many ways')")
          r (result sql)]
      (is (= expected
             (tsearch/phraseto-tsquery
              "english" "PostgreSQL can be extended by the user in many ways")))
      (is (= [[expected]] (mapv vec (.-rows ^PgWireServer$QueryResult r))))
      (is (= [types/oid-tsquery] (vec (.-columnOids ^PgWireServer$QueryResult r)))))))

(deftest typed-storage-waits-for-postgres-input-parsers
  (testing "CREATE cannot silently degrade a catalog tsearch type to text"
    (is (= "0A000" (state "CREATE TABLE bad_search_type (q tsquery)")))
    (is (= "0A000" (state "CREATE TABLE bad_search_vector (v tsvector)")))
    (is (= "0A000" (state "CREATE TABLE bad_search_array (q tsquery[])")))
    (is (= "0A000" (state "CREATE TABLE bad_search_qualified (q pg_catalog.tsquery)")))
    (is (= "0A000" (state "CREATE TABLE bad_search_quoted (q \"tsquery\")"))))
  (testing "ALTER uses the same explicit boundary"
    (result "CREATE TABLE search_type_alter (id int)")
    (is (= "0A000"
           (state "ALTER TABLE search_type_alter ADD COLUMN q tsquery"))))
  (testing "quoted case and non-catalog schemas remain distinct identifiers"
    (is (false? (types/unsupported-input-type? "\"TSQUERY\"")))
    (is (false? (types/unsupported-input-type? "application.tsquery")))))
