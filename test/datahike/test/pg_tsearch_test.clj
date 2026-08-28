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

(deftest typed-storage-boundary
  (testing "tsearch types have validated canonical text carriers"
    (result "CREATE TABLE search_vector (id int, document tsvector NOT NULL, q tsquery)")
    (result (str "INSERT INTO search_vector VALUES "
                 "(1, '''dinosaur'':7,2 ''academi'':1A', 'academi & dinosaur')"))
    (let [r (result "SELECT document, q FROM search_vector")]
      (is (= [["'academi':1A 'dinosaur':2,7"
               "'academi' & 'dinosaur'"]]
             (mapv vec (.-rows ^PgWireServer$QueryResult r))))
      (is (= [types/oid-tsvector types/oid-tsquery]
             (vec (.-columnOids ^PgWireServer$QueryResult r))))))
  (testing "quoted case and non-catalog schemas remain distinct identifiers"
    (is (false? (types/unsupported-input-type? "\"TSQUERY\"")))
    (is (false? (types/unsupported-input-type? "application.tsquery")))))

(deftest canonical-tsvector-and-tsquery-input
  (is (= "'bar' 'foo':1A,2B"
         (tsearch/canonical-tsvector "foo:2B,1A foo:2C bar")))
  (is (= "'foo' & !'bar' | 'baz':*AB"
         (tsearch/canonical-tsquery "foo&!bar|baz:BA*")))
  (is (= "42601" (:sqlstate (ex-data
                             (try (tsearch/canonical-tsvector "foo:0")
                                  (catch Exception e e))))))
  (is (= "42601" (:sqlstate (ex-data
                             (try (tsearch/canonical-tsquery "foo &")
                                  (catch Exception e e)))))))

(deftest exact-match-recheck-semantics
  (let [v "'bar':2 'bazaar':5C 'foo':1A 'weighted':7B"]
    (is (tsearch/ts-match? v "foo & bar"))
    (is (tsearch/ts-match? v "foo <-> bar"))
    (is (not (tsearch/ts-match? v "bar <-> foo")))
    (is (tsearch/ts-match? v "foo <4> baz:*"))
    (is (tsearch/ts-match? v "weighted:B"))
    (is (not (tsearch/ts-match? v "weighted:A")))
    (is (tsearch/ts-match? v "foo & !missing"))
    (is (tsearch/ts-match? v "!missing"))
    (is (false? (tsearch/ts-match? v ""))))
  (testing "stripped lexemes retain boolean membership but cannot satisfy a phrase"
    (is (tsearch/ts-match? "'foo' 'bar':2" "foo"))
    (is (not (tsearch/ts-match? "'foo' 'bar':2" "foo <-> bar"))))
  (testing "unsupported complex phrase semantics fail instead of approximating"
    (is (= "0A000" (:sqlstate
                    (ex-data
                     (try (tsearch/ts-match? "'foo':1 'bar':2"
                                             "(foo | baz) <-> bar")
                          (catch Exception e e))))))))

(deftest secondary-candidate-plans-have-no-false-negative-shortcuts
  (is (= {:op :term :lexeme "foo"}
         (:query (tsearch/tsquery-candidate-plan "foo & !bar"))))
  (is (= :all (:query (tsearch/tsquery-candidate-plan "foo | !bar"))))
  (is (= :all (:query (tsearch/tsquery-candidate-plan "!foo"))))
  (is (= {:op :and
          :args [{:op :term :lexeme "foo"}
                 {:op :prefix :lexeme "bar"}]}
         (:query (tsearch/tsquery-candidate-plan "foo <-> bar:*"))))
  (is (= :none (:query (tsearch/tsquery-candidate-plan ""))))
  (is (= {:precision :recheck :recall :complete :ordering :none}
         (select-keys (tsearch/tsquery-candidate-plan "foo")
                      [:precision :recall :ordering]))))

(deftest sql-match-operator-slice
  (result "CREATE TABLE search_match (id int, document tsvector, q tsquery)")
  (result (str "INSERT INTO search_match VALUES "
               "(1, '''foo'':1A ''bar'':2', 'foo <-> bar'), "
               "(2, '''foo'':1 ''bar'':4', 'foo <-> bar'), "
               "(3, NULL, 'foo')"))
  (is (= [["1" "t"] ["2" "f"] ["3" nil]]
         (rows "SELECT id, document @@ q FROM search_match ORDER BY id")))
  (is (= [["1"]]
         (rows "SELECT id FROM search_match WHERE document @@ q ORDER BY id")))
  (let [r (result "SELECT to_tsvector('english', 'Fat cats ate rats')")]
    (is (= [["'ate':3 'cat':2 'fat':1 'rat':4"]] (mapv vec (.-rows r))))
    (is (= [types/oid-tsvector] (vec (.-columnOids r))))))
