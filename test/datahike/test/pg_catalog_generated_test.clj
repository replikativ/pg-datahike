(ns datahike.test.pg-catalog-generated-test
  "The committed PostgreSQL catalog (src/datahike/pg/pg_catalog.edn) is
   generated output: it must not be edited by hand, must match the
   release the server reports, and must be what the generator makes from
   the pinned source when that checkout is present."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [datahike.pg.gen.catalog :as gen]
            [datahike.pg.pg-catalog :as pg-catalog]
            [datahike.pg.types :as types])
  (:import [datahike.pg PgWireServer]))

(deftest committed-catalog-is-unedited
  (let [[sha body] (gen/split-file (slurp gen/output-path))]
    (is (some? body) "the file has the generator's layout")
    (is (= sha (gen/sha256 body))
        "pg_catalog.edn changed without `bb gen-catalog`")))

(deftest catalog-release-matches-the-reported-server-version
  (is (= pg-catalog/pg-version
         (re-find #"^\d+\.\d+" PgWireServer/SERVER_VERSION))))

(deftest committed-catalog-matches-the-pinned-source
  (if (.exists (io/file gen/default-source "src/include/catalog/pg_proc.dat"))
    (is (= (slurp gen/output-path)
           (gen/render (gen/generate gen/default-source)))
        "regenerate with `bb gen-catalog`")
    (println "skipping: no PostgreSQL checkout at" gen/default-source)))

(deftest derived-tables-read-the-catalog
  (testing "PostgreSQL facts the hand-written tables had wrong"
    (is (= :Z (types/oid->category types/oid-char)) "\"char\" is category Z")
    (is (= 64 (types/oid->wire-size types/oid-name)) "NAMEDATALEN"))
  (testing "array types come from pg_type.typarray"
    (is (= 1000 (types/element-oid->array-oid types/oid-bool)))
    (is (= 950 (types/typcollation types/oid-name-array))))
  (testing "aggregate signatures cover both prokind a and w"
    (is (= :ok (types/aggregate-resolution "rank" [] [])))
    (is (= :none (types/aggregate-resolution "sum" [types/oid-text] [false])))))
