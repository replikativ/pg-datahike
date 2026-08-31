(ns datahike.test.pg-secondary-validation-test
  "End-to-end PostgreSQL access-path validation against the local secondary
   stack. The released runtime remains JDK 17 compatible; these tests activate
   only under the opt-in :local-secondary-stack alias (JDK 22+)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *handler* nil)

(def ^:private secondary-stack-probe
  (try
    (require 'datahike.index.secondary.scriptum)
    (require 'datahike.index.secondary.proximum)
    (try
      ;; Loading a legacy adapter is not enough: local-root profiles can point
      ;; at an unrelated sibling branch and otherwise produce a plausible but
      ;; meaningless green run. Pin one API introduced by each PR in this
      ;; validation vertical so stale worktrees fail loudly.
      (requiring-resolve 'datahike.index.secondary/candidate-page)
      (requiring-resolve 'proximum.generations/open-generation)
      (requiring-resolve 'scriptum.core/begin-generation)
      {:available? true}
      (catch Throwable failure
        {:available? false
         :stale? true
         :error (ex-message failure)}))
    (catch Throwable _ {:available? false :stale? false})))

(def ^:private secondary-stack-available?
  (:available? secondary-stack-probe))

(defn- secondary-stack-unavailable-assertion []
  (is (not (:stale? secondary-stack-probe))
      (str ":local-secondary-stack resolved legacy or unrelated sibling branches: "
           (:error secondary-stack-probe))))

(defn- fixture [f]
  (if-not secondary-stack-available?
    (f)
    (let [cfg {:store {:backend :memory :id (random-uuid)}
               :writer {:backend :self :writer-ownership :exclusive}
               :schema-flexibility :write
               :allow-index-backfill? true
               :max-string-length 0}
          vector-store-id (random-uuid)]
      (d/create-database cfg)
      (let [conn (d/connect cfg)
            handler
            (pg/make-query-handler
             conn
             {:secondary-index-config
              {:proximum {:store-config {:backend :memory
                                         :id vector-store-id}}
               :stratum {}}})]
        (try
          (binding [*conn* conn *handler* handler] (f))
          (finally
            (.close handler)
            (d/release conn)
            (d/delete-database cfg)))))))

(use-fixtures :each fixture)

(defn- result [sql]
  (.execute *handler* sql))

(defn- rows [sql]
  (mapv vec (.-rows ^PgWireServer$QueryResult (result sql))))

(defn- sqlstate [sql]
  (.-sqlstate ^PgWireServer$QueryResult (result sql)))

(deftest scriptum-selectivity-cost-gate
  (let [worthwhile (ns-resolve 'datahike.pg.server
                               'text-secondary-worthwhile?)
        estimate-entrypoint (ns-resolve 'datahike.pg.server
                                        'secondary-estimate-entrypoint)
        row-estimate (ns-resolve 'datahike.pg.server 'table-row-estimate)
        use? (fn [hits rows]
               (with-redefs-fn
                 {estimate-entrypoint (constantly (fn [_ _] hits))
                  row-estimate (fn [_ _] rows)}
                 #(worthwhile ::db ::index :docs/body ::query)))]
    (is (use? 2000 10000) "the measured twenty-percent boundary uses Scriptum")
    (is (not (use? 2001 10000)) "a larger candidate relation stays primary")
    (is (use? 64 100) "small absolute result sets retain the secondary path")
    (is (not (use? 65 100)))
    (is (with-redefs-fn {estimate-entrypoint (constantly nil)}
          #(worthwhile ::db ::index :docs/body ::query))
        "an older adapter without estimates preserves the compatibility path")))

(deftest unindexed-vector-equality-cost-gate
  (let [likely-small? (ns-resolve 'datahike.pg.server
                                  'likely-small-unindexed-vector-equality?)
        estimate-entrypoint (ns-resolve 'datahike.pg.server
                                        'pattern-estimate-entrypoint)
        row-estimate (ns-resolve 'datahike.pg.server 'table-row-estimate)
        schema-fn (ns-resolve 'datahike.db.interface '-schema)
        use? (fn [hits rows]
               (with-redefs-fn
                 {estimate-entrypoint (constantly (fn [& _] hits))
                  row-estimate (fn [_ _] rows)
                  schema-fn (fn [_] {:docs/category {}})}
                 #(likely-small? ::db
                                 {:kind :unindexed
                                  :attribute :docs/category
                                  :value 7}
                                 10)))]
    (is (use? 1000 100000)
        "a sampled one-percent equality skips the wasted ANN probe")
    (is (not (use? 5000 100000))
        "a common equality retains the bounded ANN-first path")
    (is (not (use? 1000 10000))
        "the relative selectivity cap prevents a ten-percent primary pass")))

(deftest scalar-candidate-pages-enforce-contract-and-close
  (let [restrict (ns-resolve 'datahike.pg.server
                             'restrict-to-scalar-order-candidates)
        candidate-entrypoint (ns-resolve 'datahike.pg.server
                                         'candidate-page-entrypoint)
        close-entrypoint (ns-resolve 'datahike.pg.server
                                     'close-candidate-scan-entrypoint)
        native-order (ns-resolve 'datahike.pg.server
                                 'native-avet-order-candidates)
        matching (ns-resolve 'datahike.pg.server
                             'matching-stratum-secondary)
        query {:find ['?e] :in ['$] :where []}
        spec {:entity-var '?e
              :attribute :docs/rank
              :direction :asc
              :candidate-limit 1}
        closes (atom [])
        redefine-base
        {native-order (constantly nil)
         matching (fn [_ _] [:idx/rank ::index])
         close-entrypoint (constantly
                           (fn [index continuation]
                             (swap! closes conj [index continuation])))}]
    (let [page {:candidates [{:entity-id 7 :attribute :docs/rank :value 10}]
                :precision :exact
                :recall :complete
                :ordering :exact
                :exhausted? false
                :continuation false}
          [restricted args access]
          (with-redefs-fn
            (assoc redefine-base candidate-entrypoint (constantly
                                                       (fn [& _] page)))
            #(restrict ::db query [] spec))]
      (is (= [[7]] args))
      (is (= :stratum-order (:kind access)))
      (is (= [['?e '...]] (take-last 1 (:in restricted))))
      (is (= [[::index false]] @closes)
          "false is a valid opaque continuation and is still closed"))
    (reset! closes [])
    (let [page {:candidates [{:entity-id 7 :attribute :docs/rank :value 10}]
                :precision :approximate
                :recall :bounded
                :ordering :approximate
                :exhausted? false
                :continuation :opaque}
          [restricted args access]
          (with-redefs-fn
            (assoc redefine-base candidate-entrypoint (constantly
                                                       (fn [& _] page)))
            #(restrict ::db query [] spec))]
      (is (= query restricted))
      (is (= [] args))
      (is (nil? access))
      (is (= [[::index :opaque]] @closes)
          "a rejected declaration closes the adapter-owned scan"))
    (reset! closes [])
    (let [requests (atom [])
          candidates (mapv (fn [entity-id]
                             {:entity-id entity-id
                              :attribute :docs/rank
                              :value entity-id})
                           (range 267))
          page {:candidates candidates
                :precision :exact
                :recall :complete
                :ordering :exact
                :exhausted? true}
          [_ args access]
          (with-redefs-fn
            (assoc redefine-base
                   candidate-entrypoint
                   (constantly
                    (fn [_ _ _ _ _ request]
                      (swap! requests conj request)
                      page)))
            #(restrict ::db query [] (assoc spec :candidate-limit 267)))]
      (is (= [{:limit 267}] @requests)
          "bounded OFFSET prefixes stay in one streaming top-N page")
      (is (= 267 (count (first args))))
      (is (= :stratum-order (:kind access)))
      (reset! requests [])
      (with-redefs-fn
        (assoc redefine-base candidate-entrypoint
               (constantly
                (fn [_ _ _ _ _ request]
                  (swap! requests conj request)
                  page)))
        #(restrict ::db query [] (assoc spec :candidate-limit 1024)))
      (is (= [{:limit 1024}] @requests)
          "a large exact prefix is requested once rather than repeatedly sorting OFFSET pages"))))

(deftest stratum-btree-declines-non-order-preserving-carriers
  (if-not secondary-stack-available?
    (secondary-stack-unavailable-assertion)
    (do
      (result "CREATE TABLE secondary_numeric_order (amount numeric NOT NULL)")
      (is (= "0A000"
             (sqlstate
              "CREATE INDEX secondary_numeric_order_idx ON secondary_numeric_order (amount)")))
      (is (nil? (get-in (d/db *conn*)
                        [:schema :datahike.pg.index/secondary_numeric_order_idx]))))))

(deftest native-btree-preserves-full-width-bigint-order
  (if-not secondary-stack-available?
    (secondary-stack-unavailable-assertion)
    (do
      (result (str "CREATE TABLE secondary_bigint_order "
                   "(id int PRIMARY KEY, rank bigint NOT NULL)"))
      (result (str "INSERT INTO secondary_bigint_order VALUES "
                   "(1, 9007199254740993), (2, 9007199254740992)"))
      (let [query (str "SELECT id FROM secondary_bigint_order "
                       "ORDER BY rank ASC LIMIT 1")
            exact (rows query)]
        (is (= [["2"]] exact))
        (is (nil? (sqlstate
                   (str "CREATE INDEX secondary_bigint_order_idx "
                        "ON secondary_bigint_order (rank)"))))
        (is (= exact (rows query))
            "the AVET cursor must preserve full-width int64 ordering")
        (is (nil? (sqlstate "DROP INDEX secondary_bigint_order_idx")))
        (is (not (:db/index
                  (get-in (d/db *conn*) [:schema :secondary_bigint_order/rank])))
            "the last named SQL owner removes its AVET carrier")))))

(deftest native-btree-lifecycle-counts-named-sql-owners
  (if-not secondary-stack-available?
    (secondary-stack-unavailable-assertion)
    (do
      (result "CREATE TABLE native_owner (id int PRIMARY KEY, rank bigint NOT NULL)")
      (result "INSERT INTO native_owner VALUES (1, 10), (2, 20)")
      (result "CREATE INDEX native_owner_rank_a ON native_owner (rank)")
      (result "CREATE INDEX native_owner_rank_b ON native_owner (rank)")
      (is (:db/index (get-in (d/db *conn*) [:schema :native_owner/rank])))
      (is (nil? (get-in (d/db *conn*)
                        [:secondary-indices :datahike.pg.index/native_owner_rank_a]))
          "native catalog entries do not maintain a duplicate Stratum copy")
      (result "DROP INDEX native_owner_rank_a")
      (is (:db/index (get-in (d/db *conn*) [:schema :native_owner/rank]))
          "one remaining named owner retains AVET")
      (result "DROP INDEX native_owner_rank_b")
      (is (not (:db/index (get-in (d/db *conn*) [:schema :native_owner/rank])))
          "the final owner removes AVET"))))

(deftest native-avet-order-declines-postgres-divergent-comparators
  (if-not secondary-stack-available?
    (secondary-stack-unavailable-assertion)
    (do
      (result (str "CREATE TABLE avet_comparator_guard ("
                   "id int PRIMARY KEY, u uuid NOT NULL, "
                   "f double precision NOT NULL)"))
      ;; Install ordinary AVET entries before data exists, without introducing
      ;; a Stratum generation; this isolates native primary-index admission.
      (d/transact *conn* [{:db/ident :avet_comparator_guard/u :db/index true}
                          {:db/ident :avet_comparator_guard/f :db/index true}])
      (result (str "INSERT INTO avet_comparator_guard VALUES "
                   "(1, '00000000-0000-0000-0000-000000000000', 'Infinity'), "
                   "(2, '80000000-0000-0000-0000-000000000000', 'NaN')"))
      (is (= [["2"]]
             (rows (str "SELECT id FROM avet_comparator_guard "
                        "WHERE u > '00000000-0000-0000-0000-000000000000'::uuid "
                        "ORDER BY u LIMIT 1")))
          "PostgreSQL UUID order is unsigned, unlike java.util.UUID.compareTo")
      (is (= [["2"]]
             (rows (str "SELECT id FROM avet_comparator_guard "
                        "WHERE f > 'Infinity'::float8 ORDER BY f LIMIT 1")))
          "PostgreSQL orders NaN after positive infinity"))))

(deftest indexed-integral-ranges-expose-safe-avet-bounds
  (if-not secondary-stack-available?
    (secondary-stack-unavailable-assertion)
    (do
      (result "CREATE TABLE range_hints (id int PRIMARY KEY, nullable_n int)")
      (let [specialize (ns-resolve 'datahike.pg.server
                                   'specialize-indexed-integral-ranges)
            eid '?range_hints_eid
            id '?range_hints_id
            nullable '?range_hints_nullable_n
            p '?p1
            base {:find [eid]
                  :in ['$ p]
                  :where [[eid :range_hints/db-row-exists true]
                          [(list 'get-else '$ eid :range_hints/id :__null__) id]
                          [(list 'not= id :__null__)]
                          [(list 'datahike.pg.sql/sql-lt? id p)]]}
            specialized (specialize (d/db *conn*) base [10])
            nullable-query
            (assoc base :where
                   [[eid :range_hints/db-row-exists true]
                    [(list 'get-else '$ eid :range_hints/nullable_n :__null__)
                     nullable]
                    [(list 'not= nullable :__null__)]
                    [(list 'datahike.pg.sql/sql-lt? nullable p)]])]
        (is (some #{[eid :range_hints/id id]} (:where specialized))
            "NOT NULL indexed columns use a direct data pattern")
        (is (some #{[(list '< id p)]} (:where specialized))
            "the native predicate is an AVET bound hint")
        (is (some #{[(list 'datahike.pg.sql/sql-lt? id p)]}
                  (:where specialized))
            "the PostgreSQL comparator remains authoritative")
        (is (= nullable-query
               (specialize (d/db *conn*) nullable-query [10]))
            "nullable columns retain get-else semantics")
        (is (= base (specialize (d/db *conn*) base [nil]))
            "a NULL parameter declines the native range hint")))))

(deftest postgres-secondary-index-vertical
  (if-not secondary-stack-available?
    (secondary-stack-unavailable-assertion)
    (do
      (result (str "CREATE TABLE secondary_docs ("
                   "id int PRIMARY KEY, priority int NOT NULL, "
                   "body tsvector, embedding vector(3))"))
      ;; Build over existing data to exercise Datahike's non-blocking writer +
      ;; buffered-delta backfill path, not only the empty-index fast path.
      (result (str "INSERT INTO secondary_docs VALUES "
                   "(1, 30, '''alpha'':1', '[1,0,0]'), "
                   "(2, 10, '''beta'':1', '[0,1,0]'), "
                   "(3, 20, '''alpha'':1 ''beta'':2', '[0.9,0.1,0]')"))
      (let [native-var (ns-resolve 'datahike.pg.server
                                   'native-avet-order-candidates)
            original @native-var
            calls (atom 0)]
        (with-redefs-fn
          {native-var (fn [& args]
                        (swap! calls inc)
                        (apply original args))}
          #(is (= [["3"] ["2"]]
                  (rows "SELECT id FROM secondary_docs ORDER BY id DESC LIMIT 2"))))
        (with-redefs-fn
          {native-var (fn [& args]
                        (swap! calls inc)
                        (apply original args))}
          #(is (= [["3"] ["2"]]
                  (rows (str "SELECT id FROM secondary_docs "
                             "WHERE id > 1 AND id <= 3 "
                             "ORDER BY id DESC LIMIT 2")))))
        (is (= 2 @calls)
            "an indexed primary-key range uses the O(log N + k) AVET path"))
      (let [^PgWireServer$QueryResult index-result
            (result "CREATE INDEX secondary_docs_priority_idx ON secondary_docs (priority)")]
        (is (nil? (.-sqlstate index-result)) (.-error index-result)))
      (is (nil? (sqlstate
                 "CREATE INDEX secondary_docs_body_gin ON secondary_docs USING gin (body)")))
      (is (nil? (sqlstate
                 "CREATE INDEX secondary_docs_body_gist ON secondary_docs USING gist (body)")))
      (is (nil? (sqlstate
                 (str "CREATE INDEX secondary_docs_embedding_hnsw ON secondary_docs "
                      "USING hnsw (embedding vector_cosine_ops) "
                      "WITH (m=8, ef_construction=32)"))))

      (let [fused-var
            (ns-resolve 'datahike.pg.server
                        'run-primary-filtered-exact-vector-query)
            fused-original @fused-var
            fused-calls (atom 0)]
        (with-redefs-fn
          {fused-var (fn [& args]
                       (swap! fused-calls inc)
                       (apply fused-original args))}
          #(is (= [["1"] ["3"]]
                  (rows (str "SELECT id FROM secondary_docs WHERE id < 4 "
                             "ORDER BY embedding <=> '[1,0,0]'::vector LIMIT 2")))))
        (is (= 1 @fused-calls)
            "a complete small AVET range fuses predicate, exact distance, and top-N"))

      (let [schema (:schema (d/db *conn*))]
        (is (= :ready
               (get-in schema
                       [:datahike.pg.index/secondary_docs_body_gin
                        :db.secondary/status])))
        (is (= :ready
               (get-in schema
                       [:datahike.pg.index/secondary_docs_body_gist
                        :db.secondary/status])))
        (is (= #{[:secondary_docs/priority true]}
               (d/q '{:find [?attr ?native]
                      :where [[?index :pg/index-name "secondary_docs_priority_idx"]
                              [?index :pg/index-attr ?attr]
                              [?index :pg/index-native-avet ?native]]}
                    (d/db *conn*))))
        (is (= {:dim 3 :distance :cosine :m 8 :ef-construction 32}
               (select-keys
                (get-in schema
                        [:datahike.pg.index/secondary_docs_embedding_hnsw
                         :db.secondary/config])
                [:dim :distance :m :ef-construction]))))

      ;; PostgreSQL @@ is still authoritative after Scriptum candidate
      ;; selection, including phrase, prefix, and negation semantics.
      (let [search-var (requiring-resolve
                        'datahike.index.secondary/search-with-vt)
            original @search-var
            calls (atom 0)]
        (with-redefs-fn
          {search-var (fn [& args]
                        (swap! calls inc)
                        (apply original args))}
          #(do
             (is (= [["1"] ["3"]]
                    (rows (str "SELECT id FROM secondary_docs "
                               "WHERE body @@ 'alpha' ORDER BY id"))))
             (is (= [["3"]]
                    (rows (str "SELECT id FROM secondary_docs "
                               "WHERE body @@ 'alpha <-> beta' ORDER BY id"))))
             (is (= [["1"]]
                    (rows (str "SELECT id FROM secondary_docs "
                               "WHERE body @@ 'alpha & !beta' ORDER BY id"))))
             (is (= [["1"] ["3"]]
                    (rows (str "SELECT id FROM secondary_docs "
                               "WHERE body @@ 'alp:*' ORDER BY id"))))
             (is (= [["3"]]
                    (rows (str "SELECT id FROM secondary_docs "
                               "WHERE body @@ plainto_tsquery('english', "
                               "'alpha beta') ORDER BY id"))))
             (is (= [["3"]]
                    (rows (str "SELECT id FROM secondary_docs "
                               "WHERE body @@ phraseto_tsquery('english', "
                               "'alpha beta') ORDER BY id"))))))
        (is (pos? @calls) "SQL @@ invoked Scriptum rather than only the exact scan"))

      (let [native-var (ns-resolve 'datahike.pg.server
                                   'native-avet-order-candidates)
            original @native-var
            successful-plans (atom 0)]
        (with-redefs-fn
          {native-var (fn [& args]
                        (let [result (apply original args)]
                          (when (some? result)
                            (swap! successful-plans inc))
                          result))}
          #(do
             (is (= [["2"] ["3"]]
                    (rows (str "SELECT id FROM secondary_docs "
                               "ORDER BY priority ASC LIMIT 2"))))
             (is (= [["1"] ["3"]]
                    (rows (str "SELECT id FROM secondary_docs "
                               "ORDER BY priority DESC LIMIT 2"))))
             (is (= [["3"] ["1"]]
                    (rows (str "SELECT id FROM secondary_docs "
                               "ORDER BY priority ASC LIMIT 2 OFFSET 1"))))
             (is (= [["3"] ["1"]]
                    (rows (str "SELECT id FROM secondary_docs WHERE priority > 15 "
                               "ORDER BY priority ASC LIMIT 2"))))
             (is (= [["1"]]
                    (rows (str "SELECT id FROM secondary_docs WHERE priority > 25 "
                               "ORDER BY priority ASC LIMIT 2"))))
             (let [before @successful-plans]
               (is (= [["1"]]
                      (rows (str "SELECT id FROM secondary_docs WHERE id = 1 "
                                 "ORDER BY priority ASC LIMIT 1"))))
               (is (= before @successful-plans)
                   "an unpushed filter declines AVET rather than under-filling"))))
        (is (= 5 @successful-plans)
            "PostgreSQL-compatible integral ordering uses native AVET for top-N and ranges"))

      (let [filtered-var (requiring-resolve 'proximum.core/search-filtered)
            filtered-original @filtered-var
            candidate-var (requiring-resolve
                           'datahike.index.secondary/candidate-page)
            candidate-original @candidate-var
            search-var (requiring-resolve 'datahike.index.secondary/search-with-vt)
            search-original @search-var
            filtered-calls (atom 0)
            candidate-calls (atom 0)
            probe-calls (atom 0)]
        (with-redefs-fn
          {filtered-var (fn [& args]
                          (swap! filtered-calls inc)
                          (apply filtered-original args))
           candidate-var (fn [& args]
                           (swap! candidate-calls inc)
                           (apply candidate-original args))
           search-var (fn [& args]
                        (when (nil? (nth args 3))
                          (swap! probe-calls inc))
                        (apply search-original args))}
          #(do
             (is (= [["1"] ["3"]]
                    (rows (str "SELECT id FROM secondary_docs "
                               "ORDER BY embedding <=> '[1,0,0]'::vector LIMIT 2"))))
             (is (pos? @candidate-calls)
                 "unfiltered top-k materialized a bounded Proximum page")
             (let [before @candidate-calls]
               (is (= [["3"]]
                      (rows (str "SELECT id FROM secondary_docs WHERE priority = 20 "
                                 "ORDER BY embedding <=> '[1,0,0]'::vector LIMIT 1"))))
               (is (= before @candidate-calls)
                   "the one-shot filtered probe does not open a cursor")
               (is (zero? @probe-calls)
                   "a hard-small relation avoids a wasted ANN probe"))))
        (is (zero? @filtered-calls)
            "the hard-small AVET filter stays on the cheaper primary exact path"))

      (doseq [index-name ["secondary_docs_priority_idx"
                          "secondary_docs_body_gin"
                          "secondary_docs_body_gist"
                          "secondary_docs_embedding_hnsw"]]
        (is (nil? (sqlstate (str "DROP INDEX " index-name)))))
      (is (every? nil?
                  (map #(get-in (d/db *conn*) [:schema %])
                       [:datahike.pg.index/secondary_docs_priority_idx
                        :datahike.pg.index/secondary_docs_body_gin
                        :datahike.pg.index/secondary_docs_body_gist
                        :datahike.pg.index/secondary_docs_embedding_hnsw])))
      (is (not (:db/index
                (get-in (d/db *conn*) [:schema :secondary_docs/priority])))
          "DROP of the last SQL B-tree owner atomically removes AVET")
      (is (nil? (sqlstate "DROP INDEX IF EXISTS secondary_docs_priority_idx")))
      (is (= "42704" (sqlstate "DROP INDEX secondary_docs_priority_idx")))

      ;; DROP TABLE removes its materialized index declaration in the same
      ;; Datahike root transaction. Use an empty table here: the secondary
      ;; cascade is independent of the separate relational-schema identity
      ;; problem for dropping and recreating populated attributes while
      ;; Datahike retains their history.
      (result "CREATE TABLE secondary_empty (priority int NOT NULL)")
      (result "CREATE INDEX secondary_docs_priority_idx ON secondary_empty (priority)")
      (is (nil? (sqlstate "DROP TABLE secondary_empty")))
      (is (nil? (get-in (d/db *conn*)
                        [:schema :datahike.pg.index/secondary_docs_priority_idx]))))))

(deftest full-text-posting-pages-and-mutations-match-the-exact-path
  (if-not secondary-stack-available?
    (secondary-stack-unavailable-assertion)
    (let [query-sql (str "SELECT id FROM secondary_text_page "
                         "WHERE body @@ 'common' ORDER BY id")
          values-sql (->> (range 1 1106)
                          (map #(format "(%d, '''common'':1')" %))
                          (str/join ", "))]
      (result "CREATE TABLE secondary_text_page (id int PRIMARY KEY, body tsvector)")
      (result (str "INSERT INTO secondary_text_page VALUES " values-sql))
      (let [exact-before (rows query-sql)]
        (is (= 1105 (count exact-before)))
        (is (nil? (sqlstate
                   (str "CREATE INDEX secondary_text_page_body_gin "
                        "ON secondary_text_page USING gin (body)"))))
        (let [search-var (requiring-resolve
                          'datahike.index.secondary/search-with-vt)
              worthwhile-var (ns-resolve 'datahike.pg.server
                                         'text-secondary-worthwhile?)
              original @search-var
              calls (atom 0)]
          (with-redefs-fn
            {search-var (fn [& args]
                          (swap! calls inc)
                          (apply original args))
             worthwhile-var (constantly true)}
            #(do
               ;; The comparison with the pre-index result is the SQL-level
               ;; completeness oracle. It crosses Scriptum's historical
               ;; 1,000-hit convenience boundary and therefore also exercises
               ;; continuation paging in Datahike's external-engine executor.
               (is (= exact-before (rows query-sql)))

               ;; A ready generation must track all three delta shapes. The
               ;; final no-index query below is authoritative, so this does not
               ;; bake adapter implementation details into the assertion.
               (result "UPDATE secondary_text_page SET body = '''rare'':1' WHERE id = 1105")
               (result "DELETE FROM secondary_text_page WHERE id = 1104")
               (result "INSERT INTO secondary_text_page VALUES (1106, '''common'':1')")
               (let [indexed-after (rows query-sql)]
                 (is (= 1104 (count indexed-after)))
                 (is (nil? (sqlstate "DROP INDEX secondary_text_page_body_gin")))
                 (is (= indexed-after (rows query-sql))))))
          (is (<= 2 @calls)
              "both indexed snapshots were served through Scriptum"))))))

(deftest filtered-vector-native-fallback-matches-the-exact-path
  (if-not secondary-stack-available?
    (secondary-stack-unavailable-assertion)
    (let [near-rejected (for [id (range 1 161)]
                          (format "(%d, 0, '[%d,0,0]')" id id))
          far-accepted (for [id (range 161 461)]
                         (format "(%d, 1, '[%d,0,0]')"
                                 id (+ 1000 (- id 161))))
          null-accepted (for [id (range 461 481)]
                          (format "(%d, 1, NULL)" id))
          values-sql (str/join ", "
                               (concat near-rejected far-accepted null-accepted))
          query-sql (str "SELECT id, bucket, bucket FROM secondary_vector_filter "
                         "WHERE bucket = 1 "
                         "ORDER BY embedding <-> '[0,0,0]'::vector "
                         "LIMIT 10 OFFSET 5")]
      (result (str "CREATE TABLE secondary_vector_filter ("
                   "id int PRIMARY KEY, bucket int NOT NULL, embedding vector(3))"))
      (result (str "INSERT INTO secondary_vector_filter VALUES " values-sql))
      (let [exact-rows (rows query-sql)]
        (is (= 10 (count exact-rows)))
        (is (every? (fn [[_ bucket-a bucket-b]]
                      (= "1" bucket-a bucket-b))
                    exact-rows)
            "duplicate projected columns survive SQL bag semantics")
        (is (nil? (sqlstate
                   (str "CREATE INDEX secondary_vector_filter_hnsw "
                        "ON secondary_vector_filter USING hnsw "
                        "(embedding vector_l2_ops) "
                        "WITH (m=16, ef_construction=128)"))))
        (is (nil? (sqlstate "SET hnsw.ef_search = 1000")))
        (let [filtered-var (requiring-resolve 'proximum.core/search-filtered)
              original @filtered-var
              filtered-calls (atom 0)]
          (with-redefs-fn
            {filtered-var (fn [& args]
                            (swap! filtered-calls inc)
                            (apply original args))}
            #(is (= exact-rows (rows query-sql))
                 "native filtered HNSW preserves exact recheck/OFFSET output"))
          (is (zero? @filtered-calls)
              "the hard-small relation stays on one authoritative query"))))))

(deftest secondary-index-ddl-rejections-are-explicit
  (if-not secondary-stack-available?
    (secondary-stack-unavailable-assertion)
    (do
      (result (str "CREATE TABLE secondary_bad (body tsvector, embedding vector, "
                   "embedding3 vector(3), embedding2001 vector(2001))"))
      ;; This handler is configured to materialize B-trees with Stratum. A
      ;; materialized generation (Stratum or Proximum) cannot escape rollback
      ;; or build against a speculative primary snapshot, so both fail closed.
      ;; The ordinary, unconfigured B-tree compatibility path remains
      ;; transaction-safe and is covered by the Chinook multi-statement test.
      (result "BEGIN")
      (is (= "0A000"
             (sqlstate
              "CREATE INDEX secondary_bad_body_btree ON secondary_bad (body)")))
      (is (= "0A000"
             (sqlstate
              (str "CREATE INDEX secondary_bad_vector_tx ON secondary_bad "
                   "USING hnsw (embedding3 vector_l2_ops)"))))
      (result "ROLLBACK")

      (is (= "0A000"
             (sqlstate (str "CREATE INDEX secondary_bad_vector_hnsw ON secondary_bad "
                            "USING hnsw (embedding vector_l2_ops)"))))
      (is (= "0A000"
             (sqlstate (str "CREATE INDEX secondary_bad_vector_ivf ON secondary_bad "
                            "USING ivfflat (embedding vector_l2_ops)"))))
      (is (= "0A000"
             (sqlstate (str "CREATE UNIQUE INDEX secondary_bad_vector_unique "
                            "ON secondary_bad USING hnsw "
                            "(embedding3 vector_l2_ops)"))))
      (is (= "0A000"
             (sqlstate (str "CREATE INDEX secondary_bad_vector_wide ON secondary_bad "
                            "USING hnsw (embedding2001 vector_l2_ops)"))))
      (is (= "0A000"
             (sqlstate (str "CREATE UNIQUE INDEX secondary_bad_body_unique "
                            "ON secondary_bad USING gin (body)"))))
      (doseq [[name options]
              [["m_low" "m=1"]
               ["m_high" "m=101"]
               ["ef_low" "ef_construction=3"]
               ["ef_high" "ef_construction=1001"]
               ["ef_below_m" "m=16, ef_construction=31"]
               ["unknown" "unknown_option=1"]
               ["non_integer" "m=wide"]]]
        (is (= "22023"
               (sqlstate
                (str "CREATE INDEX secondary_bad_" name " ON secondary_bad "
                     "USING hnsw (embedding3 vector_l2_ops) WITH (" options ")"))))))))
