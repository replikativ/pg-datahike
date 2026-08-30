(ns datahike.test.pg-vector-test
  "A focused, source-pinned foundation from pgvector 0.8.6's
   test/sql/vector_type.sql. Scalar semantics define the authoritative
   recheck contract; HNSW is exercised only as an optional candidate source."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.db.interface :as dbi]
            [datahike.pg.server :as pg]
            [datahike.pg.sql :as sql]
            [datahike.pg.types :as types]
            [datahike.pg.vector :as vector])
  (:import [datahike.pg PgParamCodec PgWireServer$PgProtocolException
            PgWireServer$QueryResult]
           [java.nio ByteBuffer ByteOrder]))

(def ^:dynamic *handler* nil)
(def ^:dynamic *conn* nil)

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0 :max-float-array-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (binding [*conn* conn
                     *handler* (pg/make-query-handler conn)]
             (f))
           (finally (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- run [sql] (.execute *handler* sql))
(defn- rows [sql] (mapv vec (.-rows ^PgWireServer$QueryResult (run sql))))
(defn- state [sql]
  (try (.-sqlstate ^PgWireServer$QueryResult (run sql))
       (catch Exception e (:sqlstate (ex-data e)))))

(defn- sqlstate [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:sqlstate (ex-data e)))))

(defn- protocol-state [f]
  (try (f) nil
       (catch PgWireServer$PgProtocolException e (.-sqlstate e))))

(deftest pgvector-086-input-and-output
  (testing "canonical examples from vector_type.sql"
    (is (= "[1,2,3]" (vector/to-pg-text (vector/parse " [ 1,  2 , 3 ] "))))
    (is (= "[1.5e+38,-1.5e-38,-0]"
           (vector/to-pg-text (vector/parse "[1.5e38,-1.5e-38,-1e-46]")))))
  (testing "syntax, finite values, range, and dimensions"
    (is (= "22P02" (sqlstate #(vector/parse "[1,,3]"))))
    (is (= "22000" (sqlstate #(vector/parse "[]"))))
    (is (= "22000" (sqlstate #(vector/parse "[NaN]"))))
    (is (= "22000" (sqlstate #(vector/parse "[Infinity]"))))
    (is (= "22003" (sqlstate #(vector/parse "[4e38]"))))
    (is (= "22000" (sqlstate #(vector/parse "[1,2,3]" 2))))
    (is (= "[1,2,3]" (vector/to-pg-text (vector/parse "[1,2,3]" 3))))))

(deftest pgvector-086-distance-semantics
  (let [a (vector/parse "[0,0]")
        b (vector/parse "[3,4]")]
    (is (= 5.0 (vector/l2-distance a b)))
    (is (= 7.0 (vector/l1-distance a b))))
  (is (= 11.0 (vector/inner-product (vector/parse "[1,2]")
                                    (vector/parse "[3,4]"))))
  (is (= -11.0 (vector/negative-inner-product (vector/parse "[1,2]")
                                              (vector/parse "[3,4]"))))
  (is (= 25.0 (vector/l2-squared-distance (vector/parse "[0,0]")
                                          (vector/parse "[3,4]"))))
  (is (= 0.0 (vector/cosine-distance (vector/parse "[1,2]")
                                     (vector/parse "[2,4]"))))
  (is (Double/isNaN (vector/cosine-distance (vector/parse "[1,2]")
                                            (vector/parse "[0,0]"))))
  (is (= "22000" (sqlstate #(vector/l2-distance (vector/parse "[1,2]")
                                                (vector/parse "[3]"))))))

(deftest vector-ddl-write-catalog-and-scalar-sql
  (run "CREATE TABLE embeddings (id int PRIMARY KEY, embedding vector(3))")
  (run "INSERT INTO embeddings VALUES (1, ' [ 1, 2, 3 ] ')")
  (is (= [["[1,2,3]" "3" "5" "5" "-25" "0"]]
         (rows (str "SELECT embedding, vector_dims(embedding), "
                    "l2_distance(embedding, '[4,6,3]'), "
                    "embedding <-> '[4,6,3]', embedding <#> '[4,6,3]', "
                    "embedding <=> '[2,4,6]' FROM embeddings"))))
  (is (= [[(str types/oid-vector) "3" "vector(3)"]]
         (rows (str "SELECT atttypid, atttypmod, "
                    "format_type(atttypid, atttypmod) FROM pg_attribute "
                    "WHERE attrelid = 'embeddings'::regclass "
                    "AND attname = 'embedding'"))))
  (is (= "22000" (state "INSERT INTO embeddings VALUES (2, '[1,2]')")))
  (is (= "22000" (state "UPDATE embeddings SET embedding = '[1]' WHERE id = 1")))
  (is (= "22P02" (state "SELECT '[1,,3]'::vector")))
  (is (= "22003" (state "SELECT '[4e38]'::vector"))))

(deftest vector-comparison-and-alter-table-semantics
  (is (= [["t" "t"]]
         (rows "SELECT '[0]'::vector = '[-0]'::vector,
                       '[1,2]'::vector < '[1,3]'::vector")))
  (run "CREATE TABLE vector_alter (id int)")
  (run "ALTER TABLE vector_alter ADD COLUMN embedding vector(2)")
  (run "INSERT INTO vector_alter VALUES (1, '[1,2]')")
  (is (= [["vector(2)"]]
         (rows (str "SELECT format_type(atttypid, atttypmod) "
                    "FROM pg_attribute WHERE attrelid = "
                    "'vector_alter'::regclass AND attname = 'embedding'"))))
  (is (= "22000"
         (state "INSERT INTO vector_alter VALUES (2, '[1,2,3]')"))))

(deftest vector-result-oids-are-known-before-execution
  (let [describe (fn [sql]
                   (let [parsed (.parse *handler* sql (int-array 0))
                         ^PgWireServer$QueryResult r (.describeResult *handler* parsed)]
                     (vec (.-columnOids r))))]
    (is (= [types/oid-vector] (describe "SELECT '[1,2]'::vector")))
    (is (= [types/oid-int4] (describe "SELECT vector_dims('[1,2]'::vector)")))
    (is (= [types/oid-float8] (describe "SELECT '[1,2]'::vector <-> '[3,4]'")))
    (is (= [types/oid-float8] (describe "SELECT cosine_distance('[1,2]'::vector, '[2,4]')")))))

(deftest nullable-vector-distance-ordering-keeps-non-null-rows
  (run "CREATE TABLE vector_nullable (id int PRIMARY KEY, embedding vector(1))")
  (run "INSERT INTO vector_nullable VALUES (1, '[2]'), (2, NULL), (3, '[1]')")
  (is (= [["3"] ["1"]]
         (rows (str "SELECT id FROM vector_nullable "
                    "ORDER BY embedding <-> '[0]'::vector LIMIT 2")))))

(deftest vector-hnsw-ddl-and-candidate-shape-are-preserved
  (run "CREATE TABLE vector_ann (id int PRIMARY KEY, embedding vector(3))")
  (let [ddl (sql/parse-sql
             (str "CREATE INDEX vector_ann_embedding_hnsw ON vector_ann "
                  "USING hnsw (embedding vector_l2_ops) "
                  "WITH (m=16, ef_construction=64)")
             {})]
    (is (= {:type :ddl-create-index
            :name "vector_ann_embedding_hnsw"
            :table "vector_ann"
            :method "hnsw"
            :unique? false
            :if-not-exists? false
            :columns ["embedding"]
            :column-specs [{:name "embedding" :params ["vector_l2_ops"]}]
            :options {:m 16 :ef_construction 64}}
           (dissoc ddl :param-count))))
  (let [db (d/db *conn*)
        parse #(sql/parse-sql % (dbi/-schema db) db)
        eligible (parse (str "SELECT id FROM vector_ann "
                             "ORDER BY embedding <-> '[1,2,3]'::vector LIMIT 5"))
        prepared (parse (str "SELECT id FROM vector_ann "
                             "ORDER BY embedding <-> $1::vector LIMIT 5"))
        unbounded (parse (str "SELECT id FROM vector_ann "
                              "ORDER BY embedding <-> '[1,2,3]'::vector"))
        descending (parse (str "SELECT id FROM vector_ann "
                               "ORDER BY embedding <-> '[1,2,3]'::vector DESC LIMIT 5"))
        nulls-first (parse (str "SELECT id FROM vector_ann "
                                "ORDER BY embedding <-> '[1,2,3]'::vector "
                                "NULLS FIRST LIMIT 5"))
        with-ties (parse (str "SELECT id FROM vector_ann "
                              "ORDER BY embedding <-> '[1,2,3]'::vector "
                              "FETCH FIRST 5 ROWS WITH TIES"))
        windowed (parse (str "SELECT id, row_number() OVER () FROM vector_ann "
                             "ORDER BY embedding <-> '[1,2,3]'::vector LIMIT 5"))
        project-set (parse (str "SELECT id, generate_series(1, 2) FROM vector_ann "
                                "ORDER BY embedding <-> '[1,2,3]'::vector LIMIT 5"))
        offset (parse (str "SELECT id FROM vector_ann "
                           "ORDER BY embedding <-> '[1,2,3]'::vector "
                           "LIMIT 5 OFFSET 7"))
        volatile-filter (parse (str "SELECT id FROM vector_ann "
                                    "WHERE random() < 0.5 "
                                    "ORDER BY embedding <-> '[1,2,3]'::vector LIMIT 5"))
        volatile-projection (parse (str "SELECT random(), id FROM vector_ann "
                                        "WHERE id > 0 "
                                        "ORDER BY embedding <-> '[1,2,3]'::vector LIMIT 5"))
        subquery-filter (parse (str "SELECT id FROM vector_ann "
                                    "WHERE id < (SELECT 10) "
                                    "ORDER BY embedding <-> '[1,2,3]'::vector LIMIT 5"))]
    (is (= {:attribute :vector_ann/embedding
            :operator "<->"
            :metric :euclidean
            :limit 5}
           (select-keys (:secondary-candidate eligible)
                        [:attribute :operator :metric :limit])))
    (is (= "[1,2,3]"
           (vector/to-pg-text
            (:query-vector (:secondary-candidate eligible)))))
    (is (sql/param-ref?
         (:query-vector (:secondary-candidate prepared))))
    (is (= 12 (get-in offset [:secondary-candidate :candidate-limit])))
    (is (nil? (:secondary-candidate unbounded)))
    (is (nil? (:secondary-candidate descending)))
    (is (nil? (:secondary-candidate nulls-first)))
    (is (nil? (:secondary-candidate with-ties)))
    (is (nil? (:secondary-candidate windowed)))
    (is (nil? (:secondary-candidate project-set)))
    (is (nil? (:secondary-candidate volatile-filter))
        "hybrid ANN must not evaluate volatile WHERE predicates repeatedly")
    (is (nil? (:secondary-candidate volatile-projection))
        "hybrid ANN must not evaluate volatile projections repeatedly")
    (is (nil? (:secondary-candidate subquery-filter))
        "nested SELECT bodies are opaque to the volatility walker")))

(deftest vector-candidates-are-only-a-rechecked-restriction
  (run (str "CREATE TABLE vector_recheck "
            "(id int PRIMARY KEY, category int, embedding vector(2))"))
  (run (str "INSERT INTO vector_recheck VALUES "
            "(1, 10, '[0,0]'), (2, 20, '[1,0]'), (3, 30, '[4,0]')"))
  (let [query-sql (str "SELECT id FROM vector_recheck "
                       "ORDER BY embedding <-> '[0.9,0]'::vector LIMIT 2")
        _ (is (= [["2"] ["1"]] (rows query-sql))
              "without a matching generation the primary scan stays exact")
        db (d/db *conn*)
        parsed (sql/parse-sql query-sql (dbi/-schema db) db)
        indexed-db (-> db
                       (assoc-in [:schema :idx/vector_recheck]
                                 {:db.secondary/type :proximum
                                  :db.secondary/attrs [:vector_recheck/embedding]
                                  :db.secondary/config {:distance :euclidean :dim 2}
                                  :db.secondary/status :ready})
                       (assoc :secondary-indices
                              {:idx/vector_recheck ::fake-index}))
        restrict (ns-resolve 'datahike.pg.server
                             'restrict-to-vector-candidates)
        [candidate-query candidate-args access]
        (restrict indexed-db (:query parsed) (:in-args parsed)
                  (:secondary-candidate parsed))
        query-spec (peek candidate-args)]
    (is (= :proximum-filter-aware (:kind access)))
    (is (= 2 (:k query-spec)))
    (is (= 40 (:ef query-spec)))
    (is (= "[0.9,0]" (vector/to-pg-text (:vector query-spec))))
    (is (= (:where (:query parsed))
           (subvec (:where candidate-query) 0 (count (:where (:query parsed)))))
        "the authoritative SQL distance and predicates remain in the query")
    (is (= [(get-in parsed [:secondary-candidate :entity-var]) '...]
           (second (peek (:where candidate-query)))))

    (testing "a WHERE-bearing candidate is deferred until the full-WHERE boundary"
      (let [filtered (sql/parse-sql
                      (str "SELECT id FROM vector_recheck WHERE id > 1 "
                           "ORDER BY embedding <-> '[0.9,0]'::vector LIMIT 2")
                      (dbi/-schema db) db)
            [filtered-query filtered-args filtered-access]
            (restrict indexed-db (:query filtered) (:in-args filtered)
                      (:secondary-candidate filtered))]
        (is (= (:query filtered) filtered-query))
        (is (= (:in-args filtered) filtered-args))
        (is (= :proximum-hybrid (:kind filtered-access)))
        (is (= 2 (get-in filtered-access [:query-spec :candidate-limit])))
        (is (= 128 (:probe-limit filtered-access)))
        (is (some? (:filtered-entrypoints filtered-access)))))

    (testing "an indexed exact value binding prefilters before probing ANN"
      (let [filtered (sql/parse-sql
                      (str "SELECT id FROM vector_recheck WHERE id = 2 "
                           "ORDER BY embedding <-> '[0.9,0]'::vector LIMIT 2")
                      (dbi/-schema db) db)
            [filtered-query filtered-args filtered-access]
            (restrict indexed-db (:query filtered) (:in-args filtered)
                      (:secondary-candidate filtered))]
        (is (= (:query filtered) filtered-query))
        (is (= (:in-args filtered) filtered-args))
        (is (= :proximum-prefiltered (:kind filtered-access))
            "the measured entity set chooses exact distance or filtered ANN")))

    (testing "a common indexed equality probes before materializing its set"
      (let [filtered (sql/parse-sql
                      (str "SELECT id FROM vector_recheck WHERE category = 20 "
                           "ORDER BY embedding <-> '[0.9,0]'::vector LIMIT 2")
                      (dbi/-schema db) db)
            estimate-var (requiring-resolve
                          'datahike.query.estimate/estimate-pattern)
            common-db (assoc-in indexed-db
                                [:schema :vector_recheck/category :db/index]
                                true)
            [_ _ filtered-access]
            (with-redefs-fn
              {estimate-var (constantly 10000)}
              #(restrict common-db (:query filtered) (:in-args filtered)
                         (:secondary-candidate filtered)))]
        (is (= :proximum-hybrid (:kind filtered-access)))
        (is (= :exact (:underfill-fallback filtered-access))
            "underfill executes one authoritative query, not a full set copy")))

    (testing "a selective unindexed equality keeps one authoritative pass"
      (let [filtered (sql/parse-sql
                      (str "SELECT id FROM vector_recheck WHERE category = 20 "
                           "ORDER BY embedding <-> '[0.9,0]'::vector LIMIT 2")
                      (dbi/-schema db) db)
            [_ _ filtered-access]
            (restrict indexed-db (:query filtered) (:in-args filtered)
                      (:secondary-candidate filtered))]
        (is (nil? filtered-access)
            "the primary sample avoids a probe plus a second O(N) pass")
        (is (= filtered-access
               (nth (restrict indexed-db (:query filtered) (:in-args filtered)
                              (:secondary-candidate filtered))
                    2))
            "the row bound is stable across repeated planning")))

    (testing "an explicit beam does not change SQL k"
      (let [[_ args _]
            (restrict indexed-db (:query parsed) (:in-args parsed)
                      (assoc (:secondary-candidate parsed) :ef 1))]
        (is (= 1 (:ef (peek args))))
        (is (= 2 (:k (peek args))))))))

(deftest iterative-vector-candidates-observe-full-query-demand-and-close
  (let [run-iterative (ns-resolve 'datahike.pg.server
                                  'run-iterative-vector-query)
        close-entrypoint (ns-resolve 'datahike.pg.server
                                     'close-candidate-scan-entrypoint)
        page-calls (atom [])
        query-eids (atom [])
        closed (atom [])
        candidate-page
        (fn [_db _ident _index _spec _filter request]
          (swap! page-calls conj request)
          (if (:continuation request)
            {:candidates [{:entity-id 3 :attribute :doc/embedding}
                          {:entity-id 4 :attribute :doc/embedding}]
             :precision :recheck
             :recall :approximate
             :ordering :exact
             :exhausted? false
             :continuation :page-2}
            {:candidates [{:entity-id 1 :attribute :doc/embedding}
                          {:entity-id 2 :attribute :doc/embedding}]
             :precision :recheck
             :recall :approximate
             :ordering :exact
             :exhausted? false
             :continuation :page-1}))
        run-query (fn [_query args]
                    (let [eids (peek args)]
                      (swap! query-eids conj eids)
                      (if (some #{4} eids) [[:enough]] [])))
        access {:candidate-page candidate-page
                :index-ident :idx/doc-embedding
                :index ::index
                :entity-var '?doc
                :attribute :doc/embedding
                :query-spec {:scan-mode :iterative}
                :page-limit 2}]
    (with-redefs-fn
      {close-entrypoint
       (constantly (fn [index continuation]
                     (swap! closed conj [index continuation])))}
      #(is (= [[:enough]]
              (run-iterative ::db {:find ['?doc]} [] run-query 1 access))))
    (is (= [{:limit 2} {:limit 2 :continuation :page-1}] @page-calls))
    (is (= [[1 2] [1 2 3 4]] @query-eids)
        "the authoritative query sees cumulative candidates after every page")
    (is (= [[::index :page-2]] @closed)
        "early LIMIT closes the still-resumable generation")))

(deftest iterative-vector-candidate-failure-falls-back-and-closes
  (let [run-iterative (ns-resolve 'datahike.pg.server
                                  'run-iterative-vector-query)
        close-entrypoint (ns-resolve 'datahike.pg.server
                                     'close-candidate-scan-entrypoint)
        page-number (atom 0)
        query-args (atom [])
        closed (atom [])
        candidate-page
        (fn [& _]
          (if (= 1 (swap! page-number inc))
            {:candidates [{:entity-id 1 :attribute :doc/embedding}]
             :precision :recheck
             :recall :approximate
             :ordering :exact
             :exhausted? false
             :continuation :page-1}
            (throw (ex-info "secondary unavailable" {:type :test/failure}))))
        run-query (fn [_query args]
                    (swap! query-args conj args)
                    (if (empty? args) [[:exact]] []))
        access {:candidate-page candidate-page
                :index-ident :idx/doc-embedding
                :index ::index
                :entity-var '?doc
                :attribute :doc/embedding
                :query-spec {:scan-mode :iterative}
                :page-limit 1}]
    (with-redefs-fn
      {close-entrypoint
       (constantly (fn [index continuation]
                     (swap! closed conj [index continuation])))}
      #(is (= [[:exact]]
              (run-iterative ::db {:find ['?doc]} [] run-query 1 access))))
    (is (= [[[1]] []] @query-args))
    (is (= [[::index :page-1]] @closed)
        "fallback releases the last live continuation")))

(deftest materialized-vector-probe-falls-back-after-full-query-underfill
  (let [run-probe (ns-resolve 'datahike.pg.server
                              'run-materialized-vector-probe)
        search-call (atom nil)
        query-call (atom nil)
        fallback-calls (atom 0)
        access {:index ::index
                :entity-var '?doc
                :query-spec {:vector [0.0 0.0]
                             :k 3
                             :candidate-limit 3}
                :probe-limit 128
                :filtered-entrypoints
                {:entity-seq identity
                 :search (fn [db index query-spec entity-filter]
                           (reset! search-call
                                   [db index query-spec entity-filter])
                           [1 2 3])}}
        run-query (fn [query args]
                    (reset! query-call [query args])
                    [])]
    (is (= [[:fallback]]
           (run-probe
            ::db {:find ['?doc]} [] run-query 1 access
            #(do (swap! fallback-calls inc) [[:fallback]]))))
    (is (= [::db ::index
            {:vector [0.0 0.0] :k 128 :candidate-limit 128}
            nil]
           @search-call))
    (is (= [[1 2 3]] (second @query-call)))
    (is (= 1 @fallback-calls))))

(deftest iterative-vector-demand-applies-offset-to-the-cumulative-recheck
  (let [run-iterative (ns-resolve 'datahike.pg.server
                                  'run-iterative-vector-query)
        close-entrypoint (ns-resolve 'datahike.pg.server
                                     'close-candidate-scan-entrypoint)
        page-number (atom 0)
        rechecked (atom [])
        closed (atom [])
        candidate-page
        (fn [& _]
          (let [n (swap! page-number inc)]
            {:candidates (mapv (fn [eid]
                                 {:entity-id eid :attribute :doc/embedding})
                               (if (= n 1) [1 2] [3 4]))
             :precision :recheck
             :recall :approximate
             :ordering :exact
             :exhausted? false
             :continuation (keyword (str "page-" n))}))
        run-query (fn [_query args]
                    (let [eids (peek args)]
                      (swap! rechecked conj eids)
                      (->> eids (drop 2) (take 2) (mapv vector))))
        access {:candidate-page candidate-page
                :index-ident :idx/doc-embedding
                :index ::index
                :entity-var '?doc
                :attribute :doc/embedding
                :query-spec {:scan-mode :iterative}
                :page-limit 2}]
    (with-redefs-fn
      {close-entrypoint
       (constantly (fn [index continuation]
                     (swap! closed conj [index continuation])))}
      #(is (= [[3] [4]]
              (run-iterative ::db {:find ['?doc]} [] run-query 2 access))))
    (is (= [[1 2] [1 2 3 4]] @rechecked))
    (is (= [[::index :page-2]] @closed))))

(deftest iterative-vector-stream-violations-fall-back-and-close
  (let [run-iterative (ns-resolve 'datahike.pg.server
                                  'run-iterative-vector-query)
        close-entrypoint (ns-resolve 'datahike.pg.server
                                     'close-candidate-scan-entrypoint)]
    (doseq [[label page]
            [["wrong attribute"
              {:candidates [{:entity-id 1 :attribute :doc/wrong}]}]
             ["duplicate candidate"
              {:candidates [{:entity-id 1 :attribute :doc/embedding}
                            {:entity-id 1 :attribute :doc/embedding}]}]
             ["empty non-exhausted page" {:candidates []}]
             ["non-exact ordering"
              {:candidates [{:entity-id 1 :attribute :doc/embedding}]
               :ordering :approximate}]]]
      (testing label
        (let [query-calls (atom [])
              closed (atom [])
              page (merge {:precision :recheck
                           :recall :approximate
                           :ordering :exact
                           :exhausted? false
                           :continuation :live}
                          page)
              access {:candidate-page (fn [& _] page)
                      :index-ident :idx/doc-embedding
                      :index ::index
                      :entity-var '?doc
                      :attribute :doc/embedding
                      :query-spec {:scan-mode :iterative}
                      :page-limit 2}
              run-query (fn [_query args]
                          (swap! query-calls conj args)
                          (if (empty? args) [[:exact]] []))]
          (with-redefs-fn
            {close-entrypoint
             (constantly (fn [index continuation]
                           (swap! closed conj [index continuation])))}
            #(is (= [[:exact]]
                    (run-iterative
                     ::db {:find ['?doc]} [] run-query 1 access))))
          (is (= [[]] @query-calls)
              "the malformed stream is never exposed to SQL recheck")
          (is (= [[::index :live]] @closed)))))))

(deftest iterative-vector-rejects-non-adjacent-continuation-cycles
  (let [run-iterative (ns-resolve 'datahike.pg.server
                                  'run-iterative-vector-query)
        close-entrypoint (ns-resolve 'datahike.pg.server
                                     'close-candidate-scan-entrypoint)
        rechecked (atom [])
        closed (atom [])
        candidate-page
        (fn [_db _ident _index _spec _filter request]
          (case (:continuation request)
            nil {:candidates [{:entity-id 1 :attribute :doc/embedding}]
                 :precision :recheck :recall :approximate :ordering :exact
                 :exhausted? false :continuation :a}
            :a {:candidates [{:entity-id 2 :attribute :doc/embedding}]
                :precision :recheck :recall :approximate :ordering :exact
                :exhausted? false :continuation :b}
            :b {:candidates [{:entity-id 3 :attribute :doc/embedding}]
                :precision :recheck :recall :approximate :ordering :exact
                :exhausted? false :continuation :a}))
        run-query (fn [_query args]
                    (swap! rechecked conj args)
                    (if (empty? args) [[:exact]] []))
        access {:candidate-page candidate-page
                :index-ident :idx/doc-embedding
                :index ::index
                :entity-var '?doc
                :attribute :doc/embedding
                :query-spec {:scan-mode :iterative}
                :page-limit 1}]
    (with-redefs-fn
      {close-entrypoint
       (constantly (fn [index continuation]
                     (swap! closed conj [index continuation])))}
      #(is (= [[:exact]]
              (run-iterative ::db {:find ['?doc]} [] run-query 1 access))))
    (is (= [[[1]] [[1 2]] []] @rechecked)
        "the cyclic page is rejected before SQL recheck")
    (is (= [[::index :a]] @closed))))

(deftest iterative-vector-treats-false-as-an-opaque-continuation
  (let [run-iterative (ns-resolve 'datahike.pg.server
                                  'run-iterative-vector-query)
        close-entrypoint (ns-resolve 'datahike.pg.server
                                     'close-candidate-scan-entrypoint)
        requests (atom [])
        closed (atom [])
        candidate-page
        (fn [_db _ident _index _spec _filter request]
          (swap! requests conj request)
          (if (contains? request :continuation)
            {:candidates [{:entity-id 2 :attribute :doc/embedding}]
             :precision :recheck :recall :approximate :ordering :exact
             :exhausted? false :continuation :live}
            {:candidates [{:entity-id 1 :attribute :doc/embedding}]
             :precision :recheck :recall :approximate :ordering :exact
             :exhausted? false :continuation false}))
        access {:candidate-page candidate-page
                :index-ident :idx/doc-embedding
                :index ::index
                :entity-var '?doc
                :attribute :doc/embedding
                :query-spec {:scan-mode :iterative}
                :page-limit 1}]
    (with-redefs-fn
      {close-entrypoint
       (constantly (fn [index continuation]
                     (swap! closed conj [index continuation])))}
      #(do
         (is (= [[:enough]]
                (run-iterative
                 ::db {:find ['?doc]} []
                 (fn [_query args]
                   (when (= [1 2] (peek args)) [[:enough]]))
                 1 access)))
         (is (= [[:first-page]]
                (run-iterative
                 ::db {:find ['?doc]} []
                 (fn [_query _args] [[:first-page]])
                 1 access)))))
    (is (= [{:limit 1}
            {:limit 1 :continuation false}
            {:limit 1}]
           @requests)
        "false resumes the first scan instead of restarting it")
    (is (= [[::index :live] [::index false]] @closed)
        "both ordinary and false live tokens are released on early LIMIT")))

(deftest vector-prefilter-removes-only-a-dead-vector-lookup
  (let [prefilter-query (ns-resolve 'datahike.pg.server
                                    'vector-prefilter-query)
        entity-var '?doc
        embedding-var '?embedding
        distance-var '?distance
        producer [(list 'get-else '$ entity-var :doc/embedding :__null__)
                  embedding-var]
        distance [(list 'vector-distance embedding-var '[0.0 0.0])
                  distance-var]
        base-query {:find ['?id distance-var]
                    :with [entity-var]
                    :where [[entity-var :doc/id '?id]
                            producer
                            distance]
                    :order-by [1 :asc]
                    :limit 10}]
    (is (= {:find [entity-var]
            :where [[entity-var :doc/id '?id]]}
           (prefilter-query base-query entity-var distance-var
                            :doc/embedding)))
    (is (= {:find [entity-var]
            :where [[entity-var :doc/id '?id]
                    producer
                    [(list 'not= embedding-var :__null__)]]}
           (prefilter-query
            (update base-query :where
                    #(vec (concat (butlast %)
                                  [[(list 'not= embedding-var :__null__)]
                                   (last %)])))
            entity-var distance-var :doc/embedding)))
    (is (= {:find [entity-var]
            :where [[entity-var :doc/id '?id] producer]}
           (prefilter-query base-query entity-var distance-var
                            :doc/other-embedding)))))

(deftest sparse-native-range-skips-the-unfiltered-vector-probe
  (let [small-range? (ns-resolve 'datahike.pg.server
                                 'small-indexed-vector-range?)
        native-candidates (ns-resolve 'datahike.pg.server
                                      'native-avet-order-candidates)
        calls (atom [])
        entity-var '?doc
        query {:find [entity-var]
               :in ['$ '?p1]
               :where [[entity-var :doc/id '?id]
                       [(list '< '?id '?p1)]]}]
    (with-redefs-fn
      {native-candidates
       (fn [& args]
         (swap! calls conj args)
         (vec (range 10)))}
      #(is (true? (small-range? ::db query [10] entity-var 10))))
    (is (= [[::db :doc/id :asc [[:< :doc/id 10]] 257]] @calls))
    (with-redefs-fn
      {native-candidates
       (fn [_db _attribute _direction _where candidate-limit]
         (vec (range candidate-limit)))}
      #(is (false? (small-range? ::db query [10] entity-var 10))))
    (reset! calls [])
    (with-redefs-fn
      {native-candidates
       (fn [& args]
         (swap! calls conj args)
         [])}
      #(do
         (is (true?
              (small-range?
               ::db (assoc query :where [[entity-var :doc/id '?id]
                                         [(list '> '?p1 '?id)]])
               [10] entity-var 10)))
         (is (false? (small-range? ::db query [10.5] entity-var 10)))))
    (is (= 1 (count @calls)))))

(deftest large-vector-prefilter-uses-the-native-entity-filter
  (let [run-prefiltered (ns-resolve 'datahike.pg.server
                                    'run-prefiltered-vector-query)
        prefilter-query* (atom nil)
        native-call* (atom nil)
        exact-call* (atom nil)
        distance-var '?distance
        entity-var '?doc
        exact-query {:find ['?id distance-var]
                     :with [entity-var]
                     :where [[entity-var :doc/id '?id]
                             [entity-var :doc/category '?category]
                             [(list 'vector-distance '?embedding '[0.0 0.0])
                              distance-var]]
                     :order-by [1 :asc]
                     :limit 10}
        filter-eids (vec (range 1 301))
        ann-eids (vec (range 1001 1011))
        run-query
        (fn
          ([query args]
           (reset! exact-call* [query args])
           [[:native-result]])
          ([query args apply-bounds?]
           (reset! prefilter-query* [query args apply-bounds?])
           (mapv vector filter-eids)))
        access
        {:index ::index
         :entity-var entity-var
         :result-var distance-var
         :query-spec {:vector [0.0 0.0]
                      :candidate-limit 10}
         :filtered-entrypoints
         {:entity-set #(do (is (= filter-eids %)) ::entity-filter)
          :entity-seq (fn [_] (throw (AssertionError. "not needed")))
          :search (fn [db index query-spec entity-filter]
                    (reset! native-call*
                            [db index query-spec entity-filter])
                    ann-eids)}}]
    (is (= [[:native-result]]
           (run-prefiltered ::db exact-query [] run-query access)))
    (let [[prefilter-query prefilter-args apply-bounds?] @prefilter-query*]
      (is (= [entity-var] (:find prefilter-query)))
      (is (= [[entity-var :doc/id '?id]
              [entity-var :doc/category '?category]]
             (:where prefilter-query)))
      (is (nil? (:with prefilter-query)))
      (is (nil? (:order-by prefilter-query)))
      (is (= [] prefilter-args))
      (is (false? apply-bounds?)))
    (is (= [::db ::index (:query-spec access) ::entity-filter]
           @native-call*))
    (let [[query args] @exact-call*]
      (is (= exact-query
             (-> query
                 (update :in pop)
                 (dissoc :in))))
      (is (= [ann-eids] args)))))

(deftest sparse-vector-prefilter-uses-native-exact-top-k
  (let [run-prefiltered (ns-resolve 'datahike.pg.server
                                    'run-prefiltered-vector-query)
        native-call* (atom nil)
        exact-call* (atom nil)
        distance-var '?distance
        entity-var '?doc
        exact-query {:find ['?id distance-var]
                     :with [entity-var]
                     :where [[entity-var :doc/id '?id]
                             [entity-var :doc/category '?category]
                             [(list 'vector-distance '?embedding '[0.0 0.0])
                              distance-var]]
                     :order-by [1 :asc]
                     :limit 10}
        filter-eids (vec (range 1 101))
        nearest-eids (vec (range 51 61))
        run-query
        (fn
          ([query args]
           (reset! exact-call* [query args])
           [[:native-exact-result]])
          ([_query _args apply-bounds?]
           (is (false? apply-bounds?))
           (mapv vector filter-eids)))
        access
        {:index ::index
         :entity-var entity-var
         :result-var distance-var
         :query-spec {:vector [0.0 0.0]
                      :candidate-limit 10}
         :filtered-entrypoints
         {:entity-set #(do (is (= filter-eids %)) ::entity-filter)
          :entity-count (fn [_] (throw (AssertionError. "sequential result")))
          :search (fn [db index query-spec entity-filter]
                    (reset! native-call*
                            [db index query-spec entity-filter])
                    nearest-eids)}}]
    (is (= [[:native-exact-result]]
           (run-prefiltered ::db exact-query [] run-query access)))
    (is (= [::db ::index
            (assoc (:query-spec access) :filter-strategy :exact)
            ::entity-filter]
           @native-call*))
    (let [[query args] @exact-call*]
      (is (= exact-query
             (-> query
                 (update :in pop)
                 (dissoc :in))))
      (is (= [nearest-eids] args)))))

(deftest vector-extension-discovery-and-binary-codec
  (is (= "CREATE EXTENSION"
         (.-commandTag ^PgWireServer$QueryResult
          (run "CREATE EXTENSION IF NOT EXISTS vector"))))
  (is (= [["vector" "0.8.6"]]
         (rows "SELECT extname, extversion FROM pg_extension WHERE extname = 'vector'")))
  (is (= "0A000" (state "CREATE EXTENSION vector")))
  (is (= [[(str types/oid-vector) "vector" "U"]]
         (rows "SELECT oid, typname, typcategory FROM pg_type WHERE typname = 'vector'")))
  (let [wire (PgParamCodec/encodeBinary types/oid-vector "[1,-0,3.5]")
        decoded (PgParamCodec/decodeBinary types/oid-vector wire)]
    (is (= "[1,-0,3.5]" (vector/to-pg-text decoded)))))

(deftest hnsw-search-beam-is-session-configurable
  (is (= [["40"]] (rows "SHOW hnsw.ef_search")))
  (is (nil? (state "SET hnsw.ef_search = 400")))
  (is (= [["400"]] (rows "SHOW hnsw.ef_search")))
  (is (nil? (state "RESET hnsw.ef_search")))
  (is (= [["40"]] (rows "SHOW hnsw.ef_search")))
  (is (= "22023" (state "SET hnsw.ef_search = 0")))
  (is (= "22023" (state "SET hnsw.ef_search = 1001")))
  (is (= "22023" (state "SET hnsw.ef_search = nope"))))

(deftest vector-analysis-and-supported-boundary
  (testing "typmods and qualified spellings survive every cast path"
    (is (= "22000" (state "SELECT '[1,2]'::vector(3)")))
    (is (= [["[1,2]"]]
           (rows "SELECT '[1,2]'::public.vector")))
    (is (= [["[1,2]"]]
           (rows "SELECT '[1,2]'::\"vector\""))))
  (testing "quoted case and non-public schemas remain distinct identifiers"
    (is (= "42704" (state "SELECT '[1]'::\"Vector\"")))
    (is (= "42704" (state "SELECT '[1]'::\"Public\".vector")))
    (is (= "42704" (state "SELECT '[1]'::other.vector"))))
  (testing "known incompatible signatures fail during analysis"
    (is (= "42883" (state "SELECT '[1]'::vector = 1")))
    (is (= "42883" (state "SELECT vector_dims(1)")))
    (is (= "42883" (state "SELECT l2_distance('[1]'::vector, 1)")))
    (is (= [["2"]]
           (rows "SELECT public.l2_distance('[1]'::vector, '[3]'::vector)")))
    (is (= "42883"
           (state "SELECT pg_catalog.l2_distance('[1]'::vector, '[3]'::vector)"))))
  (testing "unsupported storage semantics are rejected explicitly"
    (is (= "0A000" (state "CREATE TABLE vector_array_bad (v vector[])")))
    (is (= "0A000" (state "SELECT ARRAY['[1]']::vector[]")))
    (is (= "0A000" (state "CREATE TABLE vector_unique_bad (v vector UNIQUE)")))
    (run "CREATE TABLE vector_constraint_bad (v vector)")
    (is (= "0A000"
           (state "ALTER TABLE vector_constraint_bad ADD UNIQUE (v)")))
    (is (= "0A000"
           (state "CREATE INDEX vector_idx_bad ON vector_constraint_bad (v)")))
    (is (= "0A000"
           (state (str "ALTER TABLE vector_constraint_bad "
                       "ADD COLUMN vu vector UNIQUE"))))
    (is (= "0A000"
           (state (str "ALTER TABLE vector_constraint_bad "
                       "ADD COLUMN vp vector PRIMARY KEY"))))
    (is (= "42704"
           (state "ALTER TABLE vector_constraint_bad ADD COLUMN bad other.vector")))
    (is (= "42704"
           (state "ALTER TABLE vector_constraint_bad ADD COLUMN bad \"Vector\"")))
    (run "INSERT INTO vector_constraint_bad VALUES ('[0]'), ('[-0]')")
    (is (= "0A000" (state "SELECT DISTINCT v FROM vector_constraint_bad")))
    (is (= "0A000"
           (state "SELECT DISTINCT ON (v) db_id FROM vector_constraint_bad")))
    (is (= "0A000"
           (state "SELECT count(DISTINCT v) FROM vector_constraint_bad")))
    (is (= "0A000"
           (state "SELECT v, count(*) FROM vector_constraint_bad GROUP BY v")))
    (is (= "0A000"
           (state "SELECT '[0]'::vector UNION SELECT '[-0]'::vector")))
    (is (= "0A000"
           (state "SELECT '[0]'::vector INTERSECT SELECT '[-0]'::vector")))
    (is (= "0A000"
           (state "SELECT '[0]'::vector EXCEPT SELECT '[-0]'::vector")))
    (is (= [["[0]"] ["[-0]"]]
           (rows "SELECT '[0]'::vector UNION ALL SELECT '[-0]'::vector")))
    (run "BEGIN")
    (run "CREATE TABLE vector_tx_index (v vector)")
    (is (= "0A000" (state "CREATE INDEX vector_tx_idx ON vector_tx_index (v)")))
    (run "ROLLBACK")
    (is (= "0A000" (state "DROP EXTENSION vector")))))

(deftest vector-derived-ordering-and-catalog-storage
  (is (= [["[0]"] ["[1]"]]
         (rows (str "SELECT v FROM (VALUES ('[1]'::vector), "
                    "('[0]'::vector)) AS t(v) ORDER BY v"))))
  (run "CREATE TABLE vector_storage (v vector)")
  (is (= [["e"]]
         (rows (str "SELECT attstorage FROM pg_attribute WHERE attrelid = "
                    "'vector_storage'::regclass AND attname = 'v'")))))

(deftest native-float-arrays-do-not-bypass-vector-invariants
  (is (= "22000" (sqlstate #(vector/coerce (float-array 0)))))
  (is (= "22000" (sqlstate #(vector/coerce (float-array [Float/NaN])))))
  (is (= "22000"
         (sqlstate #(vector/coerce (float-array [Float/POSITIVE_INFINITY])))))
  (is (= "22000"
         (protocol-state #(PgParamCodec/encodeBinary
                           types/oid-vector (float-array 0)))))
  (is (= "22000"
         (protocol-state #(PgParamCodec/encodeBinary
                           types/oid-vector (float-array [Float/NaN]))))))

(deftest malformed-vector-binary-input-is-rejected
  (let [payload (fn [size dim unused value]
                  (let [b (doto (ByteBuffer/allocate size)
                            (.order ByteOrder/BIG_ENDIAN)
                            (.putShort (short dim))
                            (.putShort (short unused)))]
                    (when (>= size 8) (.putFloat b (float value)))
                    (.array b)))]
    (is (= "22P03"
           (protocol-state #(PgParamCodec/decodeBinary
                             types/oid-vector (byte-array 3)))))
    (is (= "22P03"
           (protocol-state #(PgParamCodec/decodeBinary
                             types/oid-vector (payload 9 1 0 0.0)))))
    (is (= "22P03"
           (protocol-state #(PgParamCodec/decodeBinary
                             types/oid-vector (payload 8 1 1 0.0)))))
    (is (= "22000"
           (protocol-state #(PgParamCodec/decodeBinary
                             types/oid-vector (payload 8 1 0 Float/NaN)))))))

(deftest vector-default-insert-select-conflict-and-returning
  (run (str "CREATE TABLE vector_paths (id int PRIMARY KEY, "
            "v vector(2) DEFAULT '[1,2]')"))
  (is (= [["[1,2]"]]
         (rows "INSERT INTO vector_paths (id) VALUES (1) RETURNING v")))
  (run "INSERT INTO vector_paths SELECT 2, '[3,4]'::vector")
  (is (= [["[5,6]"]]
         (rows (str "INSERT INTO vector_paths VALUES (2, '[5,6]') "
                    "ON CONFLICT (id) DO UPDATE SET v = EXCLUDED.v RETURNING v"))))
  (is (= [["1" "[1,2]"] ["2" "[5,6]"]]
         (rows "SELECT id, v FROM vector_paths ORDER BY id"))))
