(ns datahike.test.pg-array-subscript-read-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]
           [java.util UUID]))

(def ^:dynamic *handler* nil)

(defn- with-handler [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*handler* (pg/make-query-handler conn)] (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each with-handler)

(defn- result [sql]
  (.execute *handler* sql))

(defn- rows [sql]
  (let [^PgWireServer$QueryResult r (result sql)]
    (is (nil? (.-sqlstate r)) (str sql ": " (.-error r)))
    (mapv vec (.-rows r))))

(defn- oids [sql]
  (let [^PgWireServer$QueryResult r (result sql)]
    (is (nil? (.-sqlstate r)) (str sql ": " (.-error r)))
    (vec (.-columnOids r))))

(deftest element-reads-require-the-complete-runtime-rank
  (testing "a partial multidimensional element path is NULL, not a row array"
    (is (= [[nil "2" nil]]
           (rows (str "SELECT (ARRAY[[1,2],[3,4]])[1], "
                      "(ARRAY[[1,2],[3,4]])[1][2], "
                      "(ARRAY[[1,2],[3,4]])[1][2][1]")))))
  (testing "the complete element chain strips the base array OID exactly once"
    (is (= [23 23 23]
           (oids (str "SELECT (ARRAY[[1,2],[3,4]])[1], "
                      "(ARRAY[[1,2],[3,4]])[1][2], "
                      "(ARRAY[[1,2],[3,4]])[1][2][1]"))))))

(deftest multidimensional-slices-use-global-slice-semantics
  (is (= [["{{1,2}}" "{{2}}" "{{3}}"]]
         (rows (str "SELECT (ARRAY[[1,2],[3,4]])[1:1], "
                    "(ARRAY[[1,2],[3,4]])[1][2:2], "
                    "(ARRAY[[1,2],[3,4]])[2:2][1]"))))
  (is (= [1007 1007 1007]
         (oids (str "SELECT (ARRAY[[1,2],[3,4]])[1:1], "
                    "(ARRAY[[1,2],[3,4]])[1][2:2], "
                    "(ARRAY[[1,2],[3,4]])[2:2][1]")))))

(deftest stored-multidimensional-column-uses-the-same-complete-chain
  (rows "CREATE TABLE ars_rows (id int PRIMARY KEY, a int[][])")
  (rows "INSERT INTO ars_rows VALUES (1, ARRAY[[1,2],[3,4]])")
  (is (= [[nil "2" "{{1,2}}"]]
         (rows "SELECT a[1], a[1][2], a[1:1] FROM ars_rows WHERE id=1")))
  (is (= [23 23 1007]
         (oids "SELECT a[1], a[1][2], a[1:1] FROM ars_rows WHERE id=1"))))

(deftest stored-array-reads-use-the-declared-element-codec
  (rows "CREATE TABLE ars_text_rows (a text[])")
  (rows "INSERT INTO ars_text_rows VALUES (ARRAY['001','02'])")
  (is (= [["001" "{001,02}"]]
         (rows "SELECT a[1],a[1:2] FROM ars_text_rows")))
  (is (= [25 1009]
         (oids "SELECT a[1],a[1:2] FROM ars_text_rows"))))

(deftest slices-clamp-and-reset-lower-bounds
  ;; A retained -1 lower bound would render as `[-1:0]={7,7}`.
  (is (= [["{7,7}"]]
         (rows "SELECT (array_fill(7, ARRAY[3], ARRAY[-2]))[-1:0]")))
  (is (= [1007]
         (oids "SELECT (array_fill(7, ARRAY[3], ARRAY[-2]))[-1:0]"))))

(deftest strict-null-container-skips-index-effects
  (rows "CREATE SEQUENCE ars_null_base")
  (is (= [[nil]]
         (rows "SELECT (NULL::int[])[nextval('ars_null_base')]")))
  (is (= [["1" "f"]]
         (rows "SELECT last_value, is_called FROM ars_null_base"))))

(deftest null-index-still-evaluates-the-other-indices
  (rows "CREATE SEQUENCE ars_null_index")
  (is (= [[nil]]
         (rows (str "SELECT (ARRAY[[1,2],[3,4]])"
                    "[NULL][nextval('ars_null_index')]"))))
  (is (= [["1" "t"]]
         (rows "SELECT last_value, is_called FROM ars_null_index"))))

(deftest concrete-volatile-prefix-is-not-replayed-before-a-later-marker
  (rows "CREATE SEQUENCE ars_mixed_effects")
  (rows "SELECT setseed(0.5)")
  (is (= [["22"]]
         (rows (str "SELECT (ARRAY[[11],[22]])"
                    "[(floor(random()*2)+1)::int]"
                    "[(nextval('ars_mixed_effects')*0+1)::int]"))))
  ;; The array read consumed the first seeded draw. Replaying its concrete
  ;; random() prefix after discovering nextval would expose the third draw.
  (is (= [["0.825301858027981"]] (rows "SELECT random()"))))

(deftest read-rank-limit-is-an-analysis-error-before-index-effects
  (rows "CREATE SEQUENCE ars_maxdim")
  (let [^PgWireServer$QueryResult r
        (result (str "SELECT (NULL::int[])"
                     "[nextval('ars_maxdim')::int]"
                     "[nextval('ars_maxdim')::int]"
                     "[nextval('ars_maxdim')::int]"
                     "[nextval('ars_maxdim')::int]"
                     "[nextval('ars_maxdim')::int]"
                     "[nextval('ars_maxdim')::int]"
                     "[nextval('ars_maxdim')::int]"))]
    (is (= "54000" (.-sqlstate r))))
  (is (= [["1" "f"]] (rows "SELECT last_value,is_called FROM ars_maxdim"))))

(deftest subscript-types-are-checked-before-row-execution
  (let [^PgWireServer$QueryResult r
        (result "SELECT (ARRAY[1])['1'::text]")]
    (is (= "42804" (.-sqlstate r))))
  (is (= [["1"]] (rows "SELECT (ARRAY[1])['1']")))
  (rows "CREATE TABLE ars_text_index (i text)")
  (let [^PgWireServer$QueryResult r
        (result "SELECT (ARRAY[1])[i] FROM ars_text_index")]
    (is (= "42804" (.-sqlstate r)))))

(deftest fixed-length-raw-container-read-is-explicitly-bounded
  (let [^PgWireServer$QueryResult r (result "SELECT (point(1,2))[0]")]
    (is (= "0A000" (.-sqlstate r)))
    (is (re-find #"subscripted reads" (or (.-error r) "")))))
