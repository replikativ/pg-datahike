(ns datahike.pg.catalog.basis
  "Exact, bounded-size snapshots of stored SQL catalog inputs.

   Exact capture is accelerated by checked runtime dependency tokens. Cached
   bases retain detached values only, bounded to eight entries and 32MiB of
   structural weight (not exact JVM heap). Strict schemas use attribute-index
   ranges; read schemas require an AEVT scan to discover undeclared catalog
   attributes. The latter also visits user data."
  (:require [clojure.string :as str]
            [datahike.db.interface :as dbi]
            [datahike.db.utils :as dbu]
            [datahike.dependency-tracking :as tracking])
  (:import [java.math BigDecimal BigInteger]
           [java.util Date IdentityHashMap UUID]))

(def ^:dynamic *capture-cache*
  "Optional caller-owned IdentityHashMap, bound for one statement only.
   At most eight DB identities are retained. Mutable catalog captures bypass
   this cache; cross-statement reuse requires checked dependency tokens."
  nil)

(def default-limits
  "Structural accounting limits, not an exact JVM heap guarantee."
  {:max-datoms 100000 :max-nodes 1000000 :max-bytes 33554432 :max-depth 64})

(def temp-object-prefix
  "The namespace prefix of a session-private (temporary) catalog object.
   PostgreSQL keeps another backend's temp schema invisible; here temp
   objects are ordinary global catalog rows, so the basis has to hide
   them itself -- otherwise one session's temp DDL, including the
   cleanup at disconnect, invalidates every other session's statement."
  "__dh_pg_temp_")

(def tracking-selector
  {:attributes #{:db/ident}
   :namespaces #{"pg" "datahike.pg" "__inherit__" "__seq__"}
   :namespace-prefixes #{"datahike.pg."}
   ;; A sequence's value is not a definition, and the OID allocator is a
   ;; single counter every CREATE bumps: it says an object was allocated
   ;; somewhere, never what this statement reads. Keeping it made an
   ;; unrelated session's CREATE/DROP pair abort a live write.
   :exclude-attributes #{:__seq__/value :datahike.pg.catalog/next-oid}})

(def tracking-options {:track-dependencies {::catalog tracking-selector}})

(def ^:private token-cache-max-entries 8)
(def ^:private token-cache-max-bytes 33554432)
(def ^:private token-cache (atom []))

(defn- checked-token [db]
  (tracking/token db ::catalog tracking-selector))

(defn- config-key [db]
  (when db
    (let [config (dbi/-config db)]
      [(boolean (:attribute-refs? config)) (:schema-flexibility config)])))

(defn- cached-token-basis [key]
  (some (fn [entry] (when (= key (:key entry)) (:basis entry))) @token-cache))

(defn- cache-token-basis! [key basis]
  ;; Detached bases and opaque tokens only: never retain DB/index/storage
  ;; objects or a lazy scan. Account for a fixed per-entry overhead as well.
  (let [weight (+ 256 (get-in (meta basis) [:statistics :bytes]))]
    (when (<= weight token-cache-max-bytes)
      (swap! token-cache
             (fn [entries]
               (loop [entries (conj (into [] (remove #(= key (:key %))) entries)
                                    {:key key :basis basis :weight weight})]
                 (if (or (> (count entries) token-cache-max-entries)
                         (> (reduce + 0 (map :weight entries)) token-cache-max-bytes))
                   (recur (subvec entries 1))
                   ;; Copy after eviction: a subvector would retain evicted
                   ;; bases through its backing vector.
                   (into [] entries))))))))

(defn catalog-attribute?
  "Stored catalog namespaces, including future attributes in those namespaces.
   Sequence values are intentionally not catalog definitions, and neither
   is the OID allocator's counter (see tracking-selector). Ident datoms
   bind logical schema names to entity IDs, including schema entity ordering."
  [attr]
  (when (keyword? attr)
    (let [n (namespace attr)]
      (and (not= :__seq__/value attr)
           (not= :datahike.pg.catalog/next-oid attr)
           (or (= :db/ident attr)
               (= "pg" n)
               (= "datahike.pg" n)
               (and n (str/starts-with? n "datahike.pg."))
               (= "__inherit__" n)
               (= "__seq__" n))))))

(defn temp-object-name?
  "Does this value name a session-private catalog object? A temp table's
   storage name is `__dh_pg_temp_<session>_<name>`, carried either as a
   string or as the namespace of an attribute keyword."
  [v]
  (boolean
   (cond
     (keyword? v) (let [n (namespace v)]
                    (or (and n (str/starts-with? n temp-object-prefix))
                        (str/starts-with? (name v) temp-object-prefix)))
     (string? v) (str/starts-with? v temp-object-prefix)
     :else false)))

(defn- remove-temp-objects
  "The schema map without session-private objects, including the
   eid -> ident entries datahike keeps alongside the attribute keys."
  [schema]
  (persistent!
   (reduce-kv (fn [out k v]
                (if (or (temp-object-name? k)
                        (temp-object-name? v)
                        (and (map? v) (temp-object-name? (:db/ident v))))
                  out
                  (assoc! out k v)))
              (transient {}) schema)))

(defn- limits! [options]
  (when (or (not (map? options))
            (seq (remove (set (keys default-limits)) (keys options))))
    (throw (ex-info "Unknown catalog basis limit" {:error :catalog-basis-options})))
  (let [limits (merge default-limits options)]
    (doseq [[k v] limits]
      (when-not (and (integer? v) (pos? v) (<= v Long/MAX_VALUE))
        (throw (ex-info "Catalog basis limits must be positive integers"
                        {:error :catalog-basis-options :limit k}))))
    limits))

(defn- charge! [state limits key amount]
  (let [next (+' (get @state key 0) amount)]
    (when (> next (get limits (case key :nodes :max-nodes
                                    :bytes :max-bytes :datoms :max-datoms)))
      (throw (ex-info "Catalog basis exceeds configured limit"
                      {:error :catalog-basis-limit :limit key})))
    (vswap! state assoc key next)))

(def ^:private immutable-container-classes
  #{clojure.lang.PersistentVector clojure.lang.PersistentList
    clojure.lang.PersistentList$EmptyList clojure.lang.PersistentArrayMap
    clojure.lang.PersistentHashMap clojure.lang.PersistentTreeMap
    clojure.lang.PersistentHashSet clojure.lang.PersistentTreeSet})

(defn- immutable-container? [value]
  (loop [value value remaining 64]
    (cond
      (zero? remaining) false
      (= clojure.lang.APersistentVector$SubVector (class value))
      (recur (.-v ^clojure.lang.APersistentVector$SubVector value) (dec remaining))
      :else (contains? immutable-container-classes (class value)))))

(defn- freeze-value [value state limits depth]
  (when (> depth (:max-depth limits))
    (throw (ex-info "Catalog basis nesting exceeds configured limit"
                    {:error :catalog-basis-limit :limit :depth})))
  (charge! state limits :nodes 1)
  (charge! state limits :bytes 32)
  (let [freeze #(freeze-value % state limits (inc depth))
        concrete (class value)]
    (cond
      (or (nil? value) (#{Boolean Character UUID} concrete)) value
      (#{clojure.lang.Keyword clojure.lang.Symbol} concrete)
      (do (charge! state limits :bytes (*' 2 (+ (count (name value))
                                                (count (namespace value)))))
          ;; Symbol metadata is not part of its value and may retain an
          ;; arbitrary mutable object graph (including an entire database).
          (if (= clojure.lang.Symbol concrete) (with-meta value nil) value))
      (= String concrete)
      (do (charge! state limits :bytes (*' 2 (count value))) value)
      (instance? Date value)
      (do (vswap! state assoc :mutable? true) [:instant (.getTime ^Date value)])
      (= Double concrete) [:double (Double/doubleToRawLongBits value)]
      (= Float concrete) [:float (Float/floatToRawIntBits value)]
      (= BigInteger concrete)
      (do (charge! state limits :bytes (quot (+ 7 (.bitLength ^BigInteger value)) 8)) value)
      (= clojure.lang.BigInt concrete) (freeze (.toBigInteger ^clojure.lang.BigInt value))
      (= BigDecimal concrete)
      [:decimal (.scale ^BigDecimal value) (freeze (.unscaledValue ^BigDecimal value))]
      (= clojure.lang.Ratio concrete) [:ratio (freeze (numerator value)) (freeze (denominator value))]
      (#{Long Integer Short Byte} concrete) value
      (or (map? value) (set? value) (sequential? value))
      (do
        ;; Interfaces alone do not guarantee immutability: ArraySeq exposes
        ;; mutable backing storage, and lazy/Cons/custom collections may wrap
        ;; it. Freeze their contents, but never certify token-cache reuse.
        (when-not (immutable-container? value) (vswap! state assoc :mutable? true))
        (cond
          (map? value) [:map (into {} (map (fn [[k v]] [(freeze k) (freeze v)])) value)]
          (set? value) [:set (into #{} (map freeze) value)]
          :else [:sequence (into [] (map freeze) value)]))
      (and value (.isArray (class value)))
      (do
        (vswap! state assoc :mutable? true)
        [:array (.getName (class value))
         (mapv #(freeze (java.lang.reflect.Array/get value %))
               (range (java.lang.reflect.Array/getLength value)))])
      :else
      (throw (ex-info "Unsupported mutable or opaque catalog value"
                      {:error :catalog-basis-unsupported-value
                       :value-class (some-> value class .getName)})))))

(defn freeze-projection
  "Detach a metadata reader result under default structural limits.
   Token eligibility requires exclusively recognized immutable values;
   mutable values are detached but cannot certify subsequent token reuse."
  [value]
  (let [state (volatile! {:datoms 0 :nodes 0 :bytes 0})
        frozen (freeze-value value state default-limits 0)]
    {:value frozen
     :token-eligible? (not (:mutable? @state))
     :statistics @state}))

(defn- capture*
  "Capture exact schema/ident and reserved catalog values, excluding tx IDs.
   Metadata reports :statistics and :limits; it does not affect value equality.

   Filtered/temporal reads retain their view; this function never unwraps them.
   A caller must keep this basis paired with the plan derived from that same DB.
   Read-schema scans are intentionally unoptimized and must be measured before
   using this primitive on a serialized write path."
  ([db] (capture* db {}))
  ([db options]
   (when-not (or (nil? db) (dbu/db? db))
     (throw (ex-info "Catalog basis requires a database"
                     {:error :catalog-basis-db})))
   (when db
     (let [limits (limits! options)
           state (volatile! {:datoms 0 :nodes 0 :bytes 0})
           scanned (volatile! 0)
           schema (remove-temp-objects (dbi/-schema db))
           config (dbi/-config db)
           read? (= :read (:schema-flexibility config))
           attrs (sort (filter catalog-attribute? (conj (set (keys schema)) :db/ident)))
           rows (if read?
                  (dbi/datoms db :aevt [])
                  (mapcat #(dbi/datoms db :aevt [%]) attrs))
           rows (vec rows)
           ;; A temp object's rows are spread over several entities (the
           ;; object, its columns), and only some of them carry the name.
           ;; Drop every entity that any temp-named value belongs to.
           temp-entities (persistent!
                          (reduce (fn [out datom]
                                    (let [attr (dbi/-ident-for db (:a datom))]
                                      (if (or (temp-object-name? attr)
                                              (temp-object-name? (:v datom)))
                                        (conj! out (:e datom))
                                        out)))
                                  (transient #{}) rows))
           frozen-schema (freeze-value schema state limits 0)
           catalog
           (reduce (fn [out datom]
                     (vswap! scanned inc)
                     (let [attr (dbi/-ident-for db (:a datom))]
                       (if (and (catalog-attribute? attr)
                                (not (contains? temp-entities (:e datom))))
                         (do
                           (charge! state limits :datoms 1)
                           (conj out [(freeze-value attr state limits 0)
                                      (:e datom)
                                      (freeze-value (:v datom) state limits 0)
                                      (:added datom)]))
                         out)))
                   [] rows)]
       (with-meta {:schema frozen-schema
                   :attribute-refs? (boolean (:attribute-refs? config))
                   :schema-flexibility (:schema-flexibility config)
                 ;; Normalize numeric attribute index ordering to ident order.
                   :catalog (vec (sort-by (juxt first second) catalog))}
         {::basis true :limits limits ::token-eligible? (not (:mutable? @state))
          :statistics (assoc @state :scanned-datoms @scanned
                             :scan-mode (if read? :full-aevt :attribute-ranges))})))))

(defn capture
  "Detached exact schema/catalog comparison value; nil DB returns nil.
   See default-limits for structural bounds. Read schemas visit all AEVT
   datoms; strict schemas use catalog attribute ranges. Temporal/filter views
   are preserved. Eligible tracked snapshots share a bounded detached-basis
   cache. Mutable dates/arrays bypass both caches. Optional *capture-cache*
   reuses identical DB objects with equal limits/configuration/checked tokens
   and must be scoped by the caller to one statement."
  ([db] (capture db {}))
  ([db options]
   (when-not (or (nil? db) (dbu/db? db))
     (throw (ex-info "Catalog basis requires a database" {:error :catalog-basis-db})))
   (when (and *capture-cache* (not (instance? IdentityHashMap *capture-cache*)))
     (throw (ex-info "Catalog capture cache must be an IdentityHashMap"
                     {:error :catalog-basis-cache})))
   (let [limits (limits! options)
         token (checked-token db)
         key [token limits (config-key db)]
         compute (fn []
                   (let [captured (capture* db limits)]
                     (if (and token (::token-eligible? (meta captured)))
                       (let [captured (vary-meta captured assoc ::token token)]
                         (cache-token-basis! key captured)
                         captured)
                       captured)))
         capture-or-reuse #(or (when token (cached-token-basis key)) (compute))]
     (if (and db *capture-cache*)
       (let [^IdentityHashMap cache *capture-cache*]
         (locking cache
           (let [[cached-key captured] (.get cache db)]
             (if (= key cached-key)
               captured
               (let [captured (capture-or-reuse)]
                 ;; A mutable value may change in place even while the DB
                 ;; identity and dependency token stay unchanged.
                 (when (::token-eligible? (meta captured))
                   (when (>= (.size cache) 8) (.clear cache))
                   (.put cache db [key captured]))
                 captured)))))
       (capture-or-reuse)))))

(defn matches?
  "Compare using a checked unchanged dependency token when eligible; otherwise
   compare exact detached metadata. Never trust hashes, transaction counters,
   server invalidation generations, or schema identity alone."
  [basis db]
  (when-not (::basis (meta basis))
    (throw (ex-info "Expected a captured catalog basis" {:error :catalog-basis-value})))
  (or (and (::token-eligible? (meta basis))
           (= [(:attribute-refs? basis) (:schema-flexibility basis)] (config-key db))
           (tracking/valid? (::token (meta basis)) db ::catalog tracking-selector))
      (= basis (capture db (:limits (meta basis))))))

(defn without-replayed-enum-markers
  "Normalize an expected replay basis for exact [entity label] marker adds
   intentionally omitted from the preceding transaction buffer. Never apply
   this to the writer's actual basis or to a semantic plan/cache key."
  [basis markers]
  (when-not (::basis (meta basis))
    (throw (ex-info "Expected a captured catalog basis" {:error :catalog-basis-value})))
  (if (empty? markers)
    basis
    (let [rows (:catalog basis)
          normalized (into []
                           (remove (fn [[attr eid value]]
                                     (and (= :datahike.pg.enum/unsafe-values attr)
                                          (contains? markers [eid value]))))
                           rows)]
      (if (= (count rows) (count normalized))
        basis
        (-> (assoc basis :catalog normalized)
            ;; Proofs certify the original basis, not this synthetic
            ;; expectation. Unchanged expectations retain both proofs.
            (vary-meta dissoc ::token :datahike.pg.catalog.admission/certificate))))))
