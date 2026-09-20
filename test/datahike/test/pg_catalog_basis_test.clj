(ns datahike.test.pg-catalog-basis-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.core :as dc]
            [datahike.db :as db]
            [datahike.db.interface :as dbi]
            [datahike.dependency-tracking :as tracking]
            [datahike.pg.catalog.basis :as basis]))

(def metadata-attrs
  [:pg/not-null :pg/default-kind :pg/default-value :pg/default-arg
   :pg/check-expr :pg/fk-parent-cols :datahike.pg/hidden
   :datahike.pg/for-ident :datahike.pg.object/revision
   :datahike.pg.column/typmod :datahike.pg.index/keys
   :datahike.pg.domain/check-expr :datahike.pg.enum/values
   :datahike.pg.composite/fields :datahike.pg.future/new-definition
   :__inherit__/parent :__seq__/increment])

(defn- tx [db data] (:db-after (dc/with db data)))

(defn- fixture-db
  ([] (fixture-db false :write))
  ([refs? flexibility]
   (tx (db/empty-db nil {:schema-flexibility flexibility
                         :attribute-refs? refs?
                         :index :datahike.index/persistent-set
                         :keep-history? true})
       (mapv (fn [attr] {:db/ident attr :db/valueType :db.type/string
                         :db/cardinality :db.cardinality/one})
             (concat metadata-attrs [:user/value :__seq__/value
                                     :datahike.pg.catalog/next-oid
                                     :__dh_pg_temp_9a85fd9ca4d44b7d_bang/id])))))

(deftest namespace-selection-is-extensible-but-not-a-prefix-accident
  (doseq [attr (conj metadata-attrs :db/ident :datahike.pg.catalog/version)]
    (is (basis/catalog-attribute? attr) (str attr)))
  (doseq [attr [:__seq__/value :user/value :datahike.pgx/hidden :datahike/pg
                :other/db-row-exists nil "pg/not-null"]]
    (is (not (basis/catalog-attribute? attr)) (str attr))))

(deftest replay-normalization-keeps-foreign-markers-and-semantic-bases
  (let [base (tx (fixture-db)
                 [{:db/ident :datahike.pg.enum/unsafe-values
                   :db/valueType :db.type/string
                   :db/cardinality :db.cardinality/many}])
        foreign (tx base [[:db/add -1 :datahike.pg.enum/unsafe-values "foreign"]])
        eid (:e (first (dbi/datoms foreign :aevt [:datahike.pg.enum/unsafe-values])))
        own (tx foreign [[:db/add eid :datahike.pg.enum/unsafe-values "own"]])
        semantic (basis/capture own)
        replay (basis/without-replayed-enum-markers semantic #{[eid "own"]})]
    (is (not (basis/matches? semantic foreign)))
    (is (basis/matches? replay foreign))
    (is (not (basis/matches? replay base)))
    (is (not (basis/matches? replay own)))
    (is (basis/matches? semantic own))
    (is (= semantic (basis/without-replayed-enum-markers semantic #{[eid "absent"]})))))

(deftest every-metadata-family-is-observed-with-and-without-references
  (doseq [refs? [false true]]
    (let [source (fixture-db refs? :write)
          captured (basis/capture source)]
      (doseq [attr metadata-attrs]
        (testing (str refs? " " attr)
          (let [changed (tx source [[:db/add -1 attr "new"]])]
            (is (identical? (dbi/-schema source) (dbi/-schema changed)))
            (is (not (basis/matches? captured changed)))
            (is (basis/matches? captured source))))))))

(deftest data-writes-counter-reservations-and-max-tx-are-not-catalog-definitions
  (let [source (fixture-db)
        captured (basis/capture source)
        changed (tx source [[:db/add -1 :user/value "row"]
                            [:db/add -2 :__seq__/value "reserved"]])]
    (is (not= (:max-tx source) (:max-tx changed)))
    (is (basis/matches? captured changed))
    (is (= (:scanned-datoms (:statistics (meta captured)))
           (:scanned-datoms (:statistics (meta (basis/capture changed))))))
    (is (= :attribute-ranges (:scan-mode (:statistics (meta captured)))))))

(deftest schema-and-ident-entity-bindings-are-observed
  (let [source (fixture-db)
        captured (basis/capture source)
        changed (tx source [{:db/ident :user/new :db/valueType :db.type/string
                             :db/cardinality :db.cardinality/one}])
        indexed (tx source [{:db/ident :user/value :db/index true}])]
    (is (not (basis/matches? captured changed)))
    (is (not (basis/matches? captured indexed)))
    ;; Equal schema maps alone do not certify identical native ident entities.
    (let [other (tx source [[:db/add -1 :db/ident :native/value]])]
      (is (not (basis/matches? captured other))))))

(deftest read-schema-finds-undeclared-catalog-attributes
  (let [source (fixture-db false :read)
        captured (basis/capture source)
        changed (tx source [[:db/add -1 :datahike.pg.undeclared/new "definition"]])
        rows (tx source [[:db/add -1 :user/undeclared "row"]])]
    (is (nil? (get (dbi/-schema changed) :datahike.pg.undeclared/new)))
    (is (not (basis/matches? captured changed)))
    (is (basis/matches? captured rows))
    (is (= :full-aevt (:scan-mode (:statistics (meta captured)))))
    (is (< (:scanned-datoms (:statistics (meta captured)))
           (:scanned-datoms (:statistics (meta (basis/capture rows))))))))

(deftest metadata-only-native-writes-and-restoration
  (let [source (fixture-db)
        eid (:db/id (d/entity source :user/value))
        first (tx source [[:db/add eid :pg/default-value "one"]])
        captured (basis/capture first)
        second (tx first [[:db/add eid :pg/default-value "two"]])
        restored (tx second [[:db/add eid :pg/default-value "one"]])]
    (is (identical? (dbi/-schema first) (dbi/-schema second)))
    (is (not (basis/matches? captured second)))
    (is (basis/matches? captured restored))
    (is (basis/matches? captured first))))

(deftest temporal-read-basis-keeps-the-view
  (let [source (fixture-db)
        first (tx source [[:db/add -1 :pg/default-value "old"]])
        eid (ffirst (d/q '[:find ?e :where [?e :pg/default-value "old"]] first))
        second (tx first [[:db/add eid :pg/default-value "new"]])
        historical (d/as-of second (:max-tx first))]
    (is (= (basis/capture first) (basis/capture historical)))
    (is (not= (basis/capture second) (basis/capture historical)))
    (is (map? (basis/capture (d/history second))))
    (is (nil? (basis/capture nil)))))

(deftest detached-values-and-explicit-limits
  (let [source (fixture-db false :read)
        instant (java.util.Date. 123)
        bytes (byte-array [1 2])
        before (tx source [[:db/add -1 :pg/opaque [instant bytes]]])
        captured (basis/capture before)]
    (.setTime instant 456)
    (aset-byte bytes 0 (byte 9))
    (is (not (basis/matches? captured before)))
    (is (= captured captured)))
  (let [source (fixture-db)]
    (doseq [limits [{:max-datoms 1} {:max-bytes 1} {:max-nodes 1} {:max-depth 1}]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Catalog basis"
                            (basis/capture source limits))))
    (doseq [limits [{:max-datoms 0} {:unknown 1} {:max-bytes 1.5}]]
      (is (thrown? clojure.lang.ExceptionInfo (basis/capture source limits))))))

(deftest caller-owned-cache-is-exact-bounded-and-limit-sensitive
  (let [source (fixture-db)
        cache (java.util.IdentityHashMap.)]
    (binding [basis/*capture-cache* cache]
      (let [captured (basis/capture source)]
        (is (identical? captured (basis/capture source)))
        (is (identical? captured (basis/capture source basis/default-limits)))
        (is (thrown? clojure.lang.ExceptionInfo
                     (basis/capture source {:max-datoms 1})))
        (is (= 1 (.size cache)))
        (let [changed (tx source [[:db/add -1 :pg/default-value "changed"]])]
          (is (not (basis/matches? captured changed)))
          (is (= 2 (.size cache))))
        (doseq [i (range 20)]
          (let [row-db (tx source [[:db/add -1 :user/value (str i)]])]
            (is (= captured (basis/capture row-db)))
            (is (<= (.size cache) 8))))))
    (let [a (basis/capture source)
          b (basis/capture source)]
      (is (= a b))
      (is (not (identical? a b))))))

(defn- enrolled [db]
  (:db-after (dc/with db [] nil basis/tracking-options)))

(deftest unchanged-replay-normalization-preserves-proofs-without-scans
  (let [source (enrolled (fixture-db))
        scans (atom 0)
        original @#'basis/capture*
        certificate (Object.)]
    (with-redefs-fn
      {#'basis/token-cache (atom [])
       #'basis/capture* (fn [& args] (swap! scans inc) (apply original args))}
      (fn []
        (let [captured (vary-meta (basis/capture source) assoc
                                  :datahike.pg.catalog.admission/certificate certificate)
              row-db (tx source [[:db/add -1 :user/value "row"]])]
          (is (= 1 @scans))
          (doseq [markers [#{} #{[123 "absent"]}]]
            (let [replay (basis/without-replayed-enum-markers captured markers)]
              (is (identical? captured replay))
              (is (identical? certificate
                              (:datahike.pg.catalog.admission/certificate (meta replay))))
              (is (some? (:datahike.pg.catalog.basis/token (meta replay))))
              (is (basis/matches? replay row-db))
              (is (= 1 @scans)))))))))

(deftest metadata-projections-share-detachment-and-budget-rules
  (let [immutable {:columns ["a" :table/a]}
        frozen (basis/freeze-projection immutable)
        backing (object-array ["old"])
        mutable (basis/freeze-projection (seq backing))]
    (is (:token-eligible? frozen))
    (is (pos? (:nodes (:statistics frozen))))
    (is (false? (:token-eligible? mutable)))
    (aset backing 0 "new")
    (is (not= (:value mutable) (:value (basis/freeze-projection (seq backing)))))
    (is (thrown? clojure.lang.ExceptionInfo (basis/freeze-projection (Object.))))
    (with-redefs [basis/default-limits (assoc basis/default-limits :max-nodes 1)]
      (is (thrown? clojure.lang.ExceptionInfo (basis/freeze-projection [1]))))))

(deftest tracked-lineage-reuses-detached-basis-without-repeated-scans
  (let [source (enrolled (fixture-db))
        scans (atom 0)
        original @#'basis/capture*]
    (with-redefs-fn
      {#'basis/token-cache (atom [])
       #'basis/capture* (fn [& args] (swap! scans inc) (apply original args))}
      (fn []
        (let [captured (basis/capture source)
              row-db (tx source [[:db/add -1 :user/value "row"]
                                 [:db/add -2 :__seq__/value "counter"]])]
          (is (identical? captured (basis/capture row-db)))
          (is (basis/matches? captured row-db))
          (is (= 1 @scans))
          (is (thrown? clojure.lang.ExceptionInfo
                       (basis/capture row-db {:max-datoms 1})))
          (let [changed (tx source [[:db/add -1 :pg/default-value "new"]])]
            (is (not (basis/matches? captured changed)))))))))

(deftest replaced-enrollment-policy-cannot-certify-incomplete-tracking
  (let [source (enrolled (fixture-db))
        group-id (first (keys (:track-dependencies basis/tracking-options)))
        narrowed (:db-after (dc/with source [] nil {:track-dependencies {group-id {}}}))
        captured (basis/capture narrowed)
        changed (tx narrowed [[:db/add -1 :pg/default-value "new"]])]
    (is (some? (tracking/token narrowed group-id)))
    (is (nil? (tracking/token narrowed group-id basis/tracking-selector)))
    (is (not (basis/matches? captured changed)))))

(deftest tracked-mutable-values-bypass-both-caches
  (let [instant (java.util.Date. 123)
        bytes (byte-array [1 2])
        source (enrolled (tx (fixture-db false :read)
                             [[:db/add -1 :pg/opaque [:nested [instant bytes]]]]))
        local (java.util.IdentityHashMap.)
        global (atom [])]
    (with-redefs-fn
      {#'basis/token-cache global}
      (fn []
        (binding [basis/*capture-cache* local]
          (let [captured (basis/capture source)]
            (is (zero? (.size local)))
            (is (empty? @global))
            (.setTime instant 456)
            (is (not (basis/matches? captured source)))
            (let [after-date (basis/capture source)]
              (aset-byte bytes 0 (byte 9))
              (is (not (basis/matches? after-date source))))))))))

(deftest normalized-tracked-enum-basis-never-reuses-semantic-proof
  (let [base (enrolled
              (tx (fixture-db)
                  [{:db/ident :datahike.pg.enum/unsafe-values
                    :db/valueType :db.type/string :db/cardinality :db.cardinality/many}]))
        own (tx base [[:db/add -1 :datahike.pg.enum/unsafe-values "own"]])
        eid (:e (first (dbi/datoms own :aevt [:datahike.pg.enum/unsafe-values])))
        semantic (vary-meta (basis/capture own) assoc
                            :datahike.pg.catalog.admission/certificate (Object.))
        replay (basis/without-replayed-enum-markers semantic #{[eid "own"]})]
    (is (nil? (:datahike.pg.catalog.basis/token (meta replay))))
    (is (nil? (:datahike.pg.catalog.admission/certificate (meta replay))))
    (is (basis/matches? semantic own))
    (is (not (basis/matches? replay own)))
    (is (basis/matches? replay base))))

(deftest detached-token-cache-bounds-entries-and-aggregate-weight
  (let [global (atom [])
        source (enrolled (fixture-db))]
    (with-redefs-fn
      {#'basis/token-cache global}
      (fn []
        (dotimes [_ 12]
          (basis/capture (enrolled (fixture-db)))
          (is (<= (count @global) 8))
          (is (<= (reduce + 0 (map :weight @global)) 33554432)))
        (reset! global [])
        (let [captured (basis/capture source)
              one-weight (+ 256 (get-in (meta captured) [:statistics :bytes]))]
          (with-redefs-fn
            {#'basis/token-cache-max-bytes (* 2 one-weight)}
            (fn []
              (dotimes [_ 5]
                (basis/capture (enrolled (fixture-db)))
                (is (<= (reduce + 0 (map :weight @global)) (* 2 one-weight)))))))))))

(deftest tracked-writer-intermediate-snapshots-invalidate-immediately
  (let [source (enrolled (fixture-db))
        captured (basis/capture source)
        observed (atom nil)]
    (dc/with source [[:db/add -1 :pg/default-value "new"]
                     [:db.fn/call (fn [candidate]
                                    (reset! observed (basis/matches? captured candidate))
                                    [])]])
    (is (false? @observed))
    (is (basis/matches? captured source))))

(deftest array-backed-sequences-bypass-token-and-identity-caches
  (let [backing (object-array ["old"])
        values (seq backing)
        source (enrolled (tx (fixture-db false :read)
                             [[:db/add -1 :pg/opaque values]]))
        local (java.util.IdentityHashMap.)
        global (atom [])]
    (with-redefs-fn
      {#'basis/token-cache global}
      (fn []
        (binding [basis/*capture-cache* local]
          (let [captured (basis/capture source)]
            (is (empty? @global))
            (is (zero? (.size local)))
            (aset backing 0 "new")
            (is (= ["new"] (vec values)) "Mutation really reaches the ArraySeq")
            (is (not (basis/matches? captured source)))))))))

(deftest scalar-subclasses-are-not-retained-as-detached-values
  (let [mutable-value (atom 1)
        value (proxy [java.math.BigInteger] ["1"]
                (bitLength [] @mutable-value))]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Unsupported mutable or opaque catalog value"
         (#'basis/freeze-value value (volatile! {:nodes 0 :bytes 0}) basis/default-limits 0)))))

(deftest symbol-metadata-is-not-part-of-the-detached-basis
  (let [value (with-meta 'catalog/value {:retained-db (fixture-db)
                                         :opaque (object-array ["retained"])})
        frozen (#'basis/freeze-value value (volatile! {:nodes 0 :bytes 0}) basis/default-limits 0)]
    (is (= 'catalog/value frozen))
    (is (nil? (meta frozen)))
    (is (some? (meta value)) "The caller's symbol was not modified")))

(deftest another-sessions-temp-objects-and-the-oid-counter-are-invisible
  ;; The basis is the guard a write checks before it commits. It has to
  ;; hold what the statement was lowered against, and nothing else:
  ;; PostgreSQL keeps one backend's temp schema invisible to another,
  ;; and its OID counter is not a definition. Both used to abort
  ;; unrelated writes -- a CREATE TEMP TABLE (or the DROP a disconnect
  ;; issues) in any session, and every CREATE anywhere.
  (let [source (fixture-db)
        captured (basis/capture source)]
    (testing "the OID allocator's counter"
      (is (not (basis/catalog-attribute? :datahike.pg.catalog/next-oid)))
      (is (basis/matches?
           captured
           (tx source [[:db/add -1 :datahike.pg.catalog/next-oid "17588"]]))))
    (testing "another session's temp table, created and dropped"
      (let [created (tx source [{:datahike.pg/hidden "t"
                                 :datahike.pg.object/revision
                                 "__dh_pg_temp_9a85fd9ca4d44b7d_bang"}
                                [:db/add -1 :__dh_pg_temp_9a85fd9ca4d44b7d_bang/id "1"]])]
        (is (basis/matches? captured created))
        (is (basis/matches? (basis/capture created) source))))
    (testing "a permanent object is still observed"
      (is (not (basis/matches?
                captured
                (tx source [{:datahike.pg/hidden "t"
                             :datahike.pg.object/revision "public_thing"}])))))))
