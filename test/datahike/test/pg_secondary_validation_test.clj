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

      (let [schema (:schema (d/db *conn*))]
        (is (= :ready
               (get-in schema
                       [:datahike.pg.index/secondary_docs_body_gin
                        :db.secondary/status])))
        (is (= :ready
               (get-in schema
                       [:datahike.pg.index/secondary_docs_body_gist
                        :db.secondary/status])))
        (is (= :ready
               (get-in schema
                       [:datahike.pg.index/secondary_docs_priority_idx
                        :db.secondary/status])))
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

      (let [candidate-var (requiring-resolve
                           'datahike.index.secondary/candidate-page)
            original @candidate-var
            calls (atom 0)
            query-specs (atom [])]
        (with-redefs-fn
          {candidate-var (fn [& args]
                           (swap! calls inc)
                           (swap! query-specs conj (nth args 3))
                           (apply original args))}
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
             (let [before @calls]
               (is (= [["1"]]
                      (rows (str "SELECT id FROM secondary_docs WHERE id = 1 "
                                 "ORDER BY priority ASC LIMIT 1"))))
               (is (= before @calls)
                   "an unpushed filter declines Stratum rather than under-filling"))))
        (is (pos? @calls) "SQL scalar ordering invoked Stratum candidate paging")
        (is (some #(= [[:> :priority 15]] (:where %)) @query-specs)
            "a same-key range is evaluated inside Stratum before top-N")
        (is (some #(= [[:> :priority 25]] (:where %)) @query-specs)
            "numeric plan reuse substitutes the current range boundary"))

      (let [search-var (requiring-resolve 'proximum.core/search)
            original @search-var
            filtered-var (requiring-resolve 'proximum.core/search-filtered)
            filtered-original @filtered-var
            calls (atom 0)
            filtered-calls (atom 0)]
        (with-redefs-fn
          {search-var (fn [& args]
                        (swap! calls inc)
                        (apply original args))
           filtered-var (fn [& args]
                          (swap! filtered-calls inc)
                          (apply filtered-original args))}
          #(do
             (is (= [["1"] ["3"]]
                    (rows (str "SELECT id FROM secondary_docs "
                               "ORDER BY embedding <=> '[1,0,0]'::vector LIMIT 2"))))
             (is (= [["3"]]
                    (rows (str "SELECT id FROM secondary_docs WHERE priority = 20 "
                               "ORDER BY embedding <=> '[1,0,0]'::vector LIMIT 1"))))))
        (is (pos? (+ @calls @filtered-calls))
            "SQL vector ordering invoked Proximum through the external engine")
        (is (pos? @filtered-calls)
            "a selective primary predicate reached Proximum as an entity filter"))

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
              original @search-var
              calls (atom 0)]
          (with-redefs-fn
            {search-var (fn [& args]
                          (swap! calls inc)
                          (apply original args))}
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
