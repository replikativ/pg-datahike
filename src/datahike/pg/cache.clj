(ns datahike.pg.cache
  "Identity-keyed bounded caches for schema-derived metadata.

   Several caches memoise pure derivations of a Datahike schema map
   (constraint metadata, array-element enrichment, ref-target hints).
   Within one database, consecutive snapshots share the schema map
   *object* until a DDL mints a new one — so object identity is the
   correct cache key: it distinguishes two databases whose schemas are
   structurally equal but whose ident-entity metadata (:pg/check-*,
   :pg/fk-*, …) differs.

   These caches previously used `java.util.WeakHashMap`, which compares
   keys with `.equals` — for Clojure maps that's structural equality, so
   equal-but-distinct schemas in different databases silently SHARED
   derived constraint metadata (a cross-database correctness hazard in
   the multi-db server). An LRU bound replaces weakness: each DDL mints
   a new schema object, so old generations age out of the small LRU
   instead of relying on GC."
  (:import [java.util Collections LinkedHashMap Map]))

;; The key refers to its object WEAKLY. These caches are keyed on a
;; translated plan or a schema map, and a plan closes over the database it
;; was translated against (an uncorrelated scalar subquery's evaluator
;; keeps it as a fallback, for one). A strong key therefore pinned that
;; whole database for as long as the entry lived: pg_regress drops a
;; fixture database per file, and the 512-entry shape cache kept every
;; one of them -- ~400k datoms each, 12M live datoms by file 28. The cache
;; needs only the object's identity; whoever is still using the plan (a
;; prepared statement, the parse cache) keeps it alive, and with it the
;; entry.
(deftype IdentityKey [^java.lang.ref.WeakReference ref ^int h]
  Object
  (equals [this other]
    (or (identical? this other)
        (and (instance? IdentityKey other)
             (let [o (.get ref)]
               (and (some? o)
                    (identical? o (.get ^java.lang.ref.WeakReference
                                   (.-ref ^IdentityKey other))))))))
  (hashCode [_] h))

(defn identity-key
  "Wrap `o` so map lookups compare it by object identity, without
   keeping it alive."
  ^IdentityKey [o]
  (IdentityKey. (java.lang.ref.WeakReference. o) (System/identityHashCode o)))

(defn- stale-key?
  "Whether a cache key holds an identity key whose object is gone. Such
   an entry can never be hit again, and its VALUE may hold what its key
   no longer does, so it is removed rather than left for the LRU."
  [k]
  (cond
    (instance? IdentityKey k) (nil? (.get ^java.lang.ref.WeakReference (.-ref ^IdentityKey k)))
    (sequential? k) (boolean (some stale-key? k))
    :else false))

(defn bounded-cache
  "Synchronized access-order LRU bounded at `capacity` entries. Use
   with `identity-key`-wrapped keys for identity semantics; entries whose
   identity key has been collected are swept out on every insertion."
  ^Map [capacity]
  (Collections/synchronizedMap
   (proxy [LinkedHashMap] [16 0.75 true]
     (removeEldestEntry [_]
       ;; Called on every put. It may modify the map itself, provided it
       ;; then answers false (LinkedHashMap's contract), so the sweep and
       ;; the eviction both happen here.
       (let [^LinkedHashMap this this
             it (.iterator (.entrySet this))]
         (while (.hasNext it)
           (when (stale-key? (.getKey ^java.util.Map$Entry (.next it)))
             (.remove it)))
         (when (> (.size this) (int capacity))
           (let [it (.iterator (.entrySet this))]
             (.next it)
             (.remove it)))
         false)))))
