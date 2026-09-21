(ns datahike.test.pg-catalog-drift-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.dependency-tracking :as deps]
            [datahike.pg.catalog.basis :as basis]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (let [config {:store {:backend :memory :id (random-uuid)}
                :schema-flexibility :write :keep-history? false
                :max-string-length 0}]
    (d/create-database config)
    (let [conn (d/connect config)
          handler (pg/make-query-handler conn)]
      (try
        (binding [*conn* conn *handler* handler] (f))
        (finally
          (try (.close ^PgWireServer$QueryHandler handler)
               (finally (d/release conn) (d/delete-database config))))))))

(use-fixtures :each fixture)

(defn- execute [sql]
  (.execute ^PgWireServer$QueryHandler *handler* sql))

(defn- state [^PgWireServer$QueryResult result] (.sqlstate result))

(defn- rows [^PgWireServer$QueryResult result] (mapv vec (.rows result)))

(defn- create-table! []
  (is (nil? (state (execute "CREATE TABLE drift_rows(a int, b int)"))))
  (is (some? (:db/ident (d/entity @*conn* :drift_rows/b)))))

(defn- create-native-table! []
  (d/transact *conn* [{:db/ident :native_rows/id :db/valueType :db.type/long
                       :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                      {:db/ident :native_rows/age :db/valueType :db.type/long
                       :db/cardinality :db.cardinality/one}]))

(deftest literal-insert-admission-ignores-unrelated-table-ddl
  (create-native-table!)
  (is (nil? (state (execute "BEGIN"))))
  (is (nil? (state (execute "INSERT INTO native_rows(id,age) VALUES(700,10)"))))
  (let [other (pg/make-query-handler *conn*)]
    (try
      (is (nil? (state (.execute ^PgWireServer$QueryHandler other
                                 "CREATE TABLE unrelated_rows(id int PRIMARY KEY, note text)"))))
      (is (nil? (state (.execute ^PgWireServer$QueryHandler other
                                 "INSERT INTO unrelated_rows VALUES(1,'unrelated')"))))
      (finally (.close ^PgWireServer$QueryHandler other))))
  (is (nil? (state (execute "COMMIT"))))
  (is (= [["10"]] (rows (execute "SELECT age FROM native_rows WHERE id=700")))))

(deftest literal-insert-admission-still-rejects-new-target-metadata
  (create-native-table!)
  (is (nil? (state (execute "BEGIN"))))
  (is (nil? (state (execute "INSERT INTO native_rows(id) VALUES(700)"))))
  (d/transact *conn* [[:db/add [:db/ident :native_rows/age] :pg/not-null true]])
  (is (= "40001" (state (execute "COMMIT"))))
  (is (= [] (rows (execute "SELECT id FROM native_rows")))))

(defn- require-b! []
  ;; Native vector transactions can change PG metadata independently of SQL
  ;; cache invalidation. ALTER SET NOT NULL is currently a compatibility no-op.
  (d/transact *conn* [[:db/add [:db/ident :drift_rows/b] :pg/not-null true]])
  (is (true? (:pg/not-null (d/entity @*conn* :drift_rows/b)))))

(deftest admission-enrolls-catalog-tracking-without-tracking-row-writes
  (create-table!)
  (let [id (first (keys (:track-dependencies basis/tracking-options)))
        captured (deps/token @*conn* id basis/tracking-selector)]
    (is (some? captured))
    (is (nil? (state (execute "INSERT INTO drift_rows(a,b) VALUES(1,2)"))))
    (is (deps/valid? captured @*conn* id basis/tracking-selector))
    (require-b!)
    (is (not (deps/valid? captured @*conn* id basis/tracking-selector)))))

(deftest native-selector-replacement-cannot-weaken-catalog-validation
  (create-table!)
  (let [id (first (keys (:track-dependencies basis/tracking-options)))]
    (d/transact *conn* {:tx-data []
                        :tx-options {:track-dependencies {id {}}}})
    (is (nil? (deps/token @*conn* id basis/tracking-selector)))
    ;; Populate the SQL cache while this wrongly configured group is present.
    (is (nil? (state (execute "INSERT INTO drift_rows(a) VALUES(1)"))))
    (require-b!)
    (is (= "23502" (state (execute "INSERT INTO drift_rows(a) VALUES(2)"))))))

(defn- after-planning [run-query change-catalog]
  (let [entered (promise)
        proceed (promise)
        original @#'pg/apply-column-constraints]
    (with-redefs-fn
      {#'pg/apply-column-constraints
       (fn [& args]
         (let [planned (apply original args)]
           (deliver entered true)
           (when (= ::timeout (deref proceed 30000 ::timeout))
             (throw (ex-info "Test did not release planned INSERT" {})))
           planned))}
      (fn []
        (let [pending (future (run-query))]
          (try
            (when-not (= true (deref entered 30000 ::timeout))
              (throw (ex-info "INSERT did not reach constraint planning" {})))
            (change-catalog)
            (deliver proceed true)
            (let [result (deref pending 30000 ::timeout)]
              (when (= ::timeout result)
                (throw (ex-info "Planned INSERT did not finish" {})))
              result)
            (finally
              (deliver proceed true)
              (when-not (realized? pending) (future-cancel pending)))))))))

(defn- assert-stale-insert-rejected! [run-query]
  (let [result (after-planning run-query
                               #(do (require-b!) (pg/invalidate-schema-cache!)))]
    (is (= "40001" (state result)) "Captured catalog plans must fail closed")
    (is (empty? (rows result)) "Rejected RETURNING must not expose speculative rows")
    (is (= [] (rows (execute "SELECT a,b FROM drift_rows"))))
    (is (= "23502" (state (execute "INSERT INTO drift_rows(a) VALUES(2)"))))))

(deftest simple-insert-rejects-catalog-drift
  (doseq [sql ["INSERT INTO drift_rows(a) VALUES(1)"
               "INSERT INTO drift_rows(a,b) VALUES(1,NULL)"
               "INSERT INTO drift_rows(a) VALUES(1) RETURNING a"]]
    (testing sql
      (create-table!)
      (assert-stale-insert-rejected! #(execute sql))
      (is (nil? (state (execute "DROP TABLE drift_rows")))))))

(deftest prepared-insert-rejects-catalog-drift
  (create-table!)
  (let [parsed (.parse ^PgWireServer$QueryHandler *handler*
                       "INSERT INTO drift_rows(a) VALUES($1)" (int-array [23]))
        bound (object-array [nil (int 1)])]
    (assert-stale-insert-rejected!
     #(let [result (.executePrepared ^PgWireServer$QueryHandler *handler* parsed bound)]
        ;; Extended-query execution may legally stage against its snapshot.
        ;; Sync is the publication boundary for this implicit transaction.
        (if (.error ^PgWireServer$QueryResult result)
          (do (.rollbackImplicit ^PgWireServer$QueryHandler *handler*) result)
          (or (.commitImplicit ^PgWireServer$QueryHandler *handler*)
              (PgWireServer$QueryResult/empty "COMMIT")))))))

(deftest buffered-insert-rejects-catalog-drift-at-commit
  (create-table!)
  (is (nil? (state (execute "BEGIN"))))
  (is (nil? (state (execute "INSERT INTO drift_rows(a) VALUES(1)"))))
  (require-b!)
  (pg/invalidate-schema-cache!)
  (is (= "40001" (state (execute "COMMIT"))))
  (execute "ROLLBACK")
  (is (= [] (rows (execute "SELECT a,b FROM drift_rows"))))
  (is (= "23502" (state (execute "INSERT INTO drift_rows(a) VALUES(2)")))))

(deftest fresh-insert-observes-native-metadata-without-cache-invalidation
  (create-table!)
  ;; Warm the same constraint cache with a write, then remove its row before
  ;; strengthening the constraint. No existing row violates the new rule.
  (is (nil? (state (execute "INSERT INTO drift_rows(a) VALUES(1)"))))
  (is (nil? (state (execute "DELETE FROM drift_rows"))))
  (require-b!)
  (is (= "23502" (state (execute "INSERT INTO drift_rows(a) VALUES(2)"))))
  (is (= [] (rows (execute "SELECT a,b FROM drift_rows")))))

(deftest own-table-ddl-and-insert-replay-together
  (is (nil? (state (execute "BEGIN"))))
  (is (nil? (state (execute "CREATE TABLE own_rows(a int NOT NULL)"))))
  (is (nil? (state (execute "INSERT INTO own_rows VALUES(7)"))))
  (is (nil? (state (execute "COMMIT"))))
  (is (= [["7"]] (rows (execute "SELECT a FROM own_rows")))))

(deftest own-enum-ddl-does-not-persist-speculative-unsafe-markers
  (is (nil? (state (execute "CREATE TYPE drift_mood AS ENUM ('old')"))))
  (is (nil? (state (execute "CREATE TABLE enum_rows(m drift_mood)"))))
  (is (nil? (state (execute "INSERT INTO enum_rows VALUES('old')"))))
  (is (nil? (state (execute "BEGIN"))))
  (is (nil? (state (execute "ALTER TYPE drift_mood ADD VALUE 'new'"))))
  (is (nil? (state (execute "SAVEPOINT before_unsafe_label"))))
  (is (= "55P04" (state (execute "INSERT INTO enum_rows VALUES('new')"))))
  (is (nil? (state (execute "ROLLBACK TO SAVEPOINT before_unsafe_label"))))
  (is (nil? (state (execute "SAVEPOINT before_unsafe_update"))))
  (is (= "55P04" (state (execute "UPDATE enum_rows SET m='new' WHERE m='old'"))))
  (is (nil? (state (execute "ROLLBACK TO SAVEPOINT before_unsafe_update"))))
  (is (= [["old"]] (rows (execute "SELECT m FROM enum_rows"))))
  ;; The old label remains legal while the added label is unsafe. COMMIT
  ;; deliberately drops unsafe-values facts, which are not durable catalog.
  (is (nil? (state (execute "INSERT INTO enum_rows VALUES('old')"))))
  (is (nil? (state (execute "COMMIT"))))
  (is (= [["old"] ["old"]] (rows (execute "SELECT m FROM enum_rows"))))
  (is (nil? (state (execute "INSERT INTO enum_rows VALUES('new')"))))
  (is (= {["old"] 2 ["new"] 1}
         (frequencies (rows (execute "SELECT m FROM enum_rows"))))))

(deftest foreign-unsafe-enum-marker-is-not-elided-from-replay-check
  (is (nil? (state (execute "CREATE TYPE drift_mood AS ENUM ('old')"))))
  (is (nil? (state (execute "CREATE TABLE enum_rows(m drift_mood)"))))
  (let [eid (d/q '[:find ?e . :where [?e :datahike.pg.enum/name "drift_mood"]]
                 @*conn*)
        result (after-planning
                #(execute "INSERT INTO enum_rows VALUES('old')")
                #(d/transact *conn* [[:db/add eid :datahike.pg.enum/unsafe-values "old"]]))]
    (is (= "40001" (state result)))
    (is (= [] (rows (execute "SELECT m FROM enum_rows"))))
    ;; Only this transaction's deliberately omitted marker writes may be
    ;; projected out of its expected replay basis. A native writer's marker
    ;; is real changed metadata and must remain visible to fresh plans too.
    (is (= "55P04" (state (execute "INSERT INTO enum_rows VALUES('old')"))))))

(deftest unrelated-native-data-write-does-not-invalidate-catalog
  (create-table!)
  (is (nil? (state (execute "CREATE TABLE other_rows(a int)"))))
  (let [result (after-planning
                #(execute "INSERT INTO drift_rows(a) VALUES(1)")
                #(d/transact *conn* [{:other_rows/a 9
                                      :other_rows/db-row-exists true}]))]
    (is (nil? (state result)))
    (is (= [["1" nil]] (rows (execute "SELECT a,b FROM drift_rows"))))
    (is (= [["9"]] (rows (execute "SELECT a FROM other_rows"))))))

(defn- create-sequence-table! []
  (is (nil? (state (execute "CREATE SEQUENCE drift_seq"))))
  (is (nil? (state (execute
                    "CREATE TABLE drift_rows(a int DEFAULT nextval('drift_seq'), b int)")))))

(defn- sequence-value []
  (d/q '[:find ?v . :where [?e :__seq__/name "drift_seq"]
         [?e :__seq__/value ?v]] @*conn*))

(deftest rejected-catalog-plan-does-not-repeat-sequence-reservation
  (create-sequence-table!)
  (let [before (sequence-value)
        result (after-planning
                #(execute "INSERT INTO drift_rows(a) VALUES(nextval('drift_seq'))")
                #(do (require-b!) (pg/invalidate-schema-cache!)))]
    (is (= "40001" (state result)))
    (is (= (inc before) (sequence-value)) "One attempted nextval consumes one value")
    (is (= [] (rows (execute "SELECT a,b FROM drift_rows"))))))

(defn- start-copy-mode! []
  (let [result (execute "COPY drift_rows(b) FROM STDIN")]
    (is (nil? (state result)))
    (is (.copyInMode ^PgWireServer$QueryResult result))))

(defn- start-copy! []
  (start-copy-mode!)
  (.copyChunk ^PgWireServer$QueryHandler *handler* (.getBytes "1\n" "UTF-8")))

(deftest copy-start-basis-is-checked-before-reserving-defaults
  (create-sequence-table!)
  (let [before (sequence-value)]
    (start-copy-mode!)
    (require-b!)
    (.copyChunk ^PgWireServer$QueryHandler *handler* (.getBytes "1\n" "UTF-8"))
    (let [result (.copyComplete ^PgWireServer$QueryHandler *handler*)]
      (is (= "40001" (state result)))
      (is (= before (sequence-value)) "Already-stale COPY must not reserve a default")
      (is (= [] (rows (execute "SELECT a,b FROM drift_rows")))))))

(deftest copy-writer-guard-rechecks-after-default-reservation
  (create-sequence-table!)
  (let [before (sequence-value)]
    (start-copy!)
    (let [result (after-planning
                  #(.copyComplete ^PgWireServer$QueryHandler *handler*)
                  #(do (require-b!) (pg/invalidate-schema-cache!)))]
      (is (= "40001" (state result)))
      (is (= (inc before) (sequence-value)) "COPY does not reevaluate its reserved default")
      (is (= [] (rows (execute "SELECT a,b FROM drift_rows")))))))

(deftest a-new-relation-does-not-abort-an-open-transaction
  ;; The guard compares the catalog a statement was lowered against with
  ;; the one its write lands on, and compared them for EQUALITY: another
  ;; session's `CREATE TABLE other` -- a table this transaction cannot
  ;; have read -- aborted it with 40001 at COMMIT. PostgreSQL does not.
  ;;
  ;; The difference is examined instead, and admitted only when the
  ;; catalog merely GAINED relations: nothing the capture held may be
  ;; missing or changed, and every added row must belong to an entity it
  ;; did not have.
  (create-table!)
  (is (nil? (state (execute "BEGIN"))))
  (is (nil? (state (execute "INSERT INTO drift_rows(a,b) VALUES(1,2)"))))
  ;; another session's unrelated DDL, mid-transaction
  (is (nil? (state (execute "CREATE TABLE unrelated_rows(x int)"))))
  (pg/invalidate-schema-cache!)
  (is (nil? (state (execute "UPDATE drift_rows SET b = 3 WHERE a = 1"))))
  (is (nil? (state (execute "COMMIT"))))
  (is (= [["1" "3"]] (rows (execute "SELECT a,b FROM drift_rows")))))

(deftest only-new-relations-admits-additions-and-nothing-else
  ;; The rule itself, at the value level: a capture is compatible with a
  ;; later one only when the later one ADDED relations. A column added to
  ;; a relation the capture knew is a change it may have to see -- a
  ;; default, a constraint -- so it is not admitted.
  (let [before (basis/capture (d/db *conn*))]
    (d/transact *conn* [{:db/ident :brand_new/id :db/valueType :db.type/long
                         :db/cardinality :db.cardinality/one}])
    (let [after-new (d/db *conn*)]
      (testing "a relation the capture did not know"
        (is (true? (basis/compatible? before after-new)))
        (is (false? (basis/matches? before after-new))
            "and it is genuinely not equal"))
      (testing "a column added to a relation it did know"
        (let [mid (basis/capture after-new)]
          (d/transact *conn* [{:db/ident :brand_new/extra :db/valueType :db.type/long
                               :db/cardinality :db.cardinality/one}])
          (is (false? (basis/compatible? mid (d/db *conn*)))))))))

