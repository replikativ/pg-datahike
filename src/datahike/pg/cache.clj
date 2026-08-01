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

(deftype IdentityKey [obj ^int h]
  Object
  (equals [_ other]
    (and (instance? IdentityKey other)
         (identical? obj (.-obj ^IdentityKey other))))
  (hashCode [_] h))

(defn identity-key
  "Wrap `o` so map lookups compare it by object identity."
  ^IdentityKey [o]
  (IdentityKey. o (System/identityHashCode o)))

(defn bounded-cache
  "Synchronized access-order LRU bounded at `capacity` entries. Use
   with `identity-key`-wrapped keys for identity semantics."
  ^Map [capacity]
  (Collections/synchronizedMap
   (proxy [LinkedHashMap] [16 0.75 true]
     (removeEldestEntry [_]
       (> (.size ^LinkedHashMap this) (int capacity))))))
