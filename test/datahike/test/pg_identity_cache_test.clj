(ns datahike.test.pg-identity-cache-test
  "The identity-keyed caches must not keep their keys' objects alive.

   They are keyed on translated plans and schema maps, and a plan closes
   over the database it was translated against. With a strong key the
   select-shape cache pinned every database a cached plan had seen:
   pg_regress drops a fixture database per file, and the server's live
   heap grew by one fixture -- ~400k datoms -- per file until the harness
   restarted it."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.pg.cache :as pg-cache]))

(defn- collect!
  "Run the collector until `ref` clears, or give up."
  [^java.lang.ref.WeakReference ref]
  (loop [i 0]
    (when (and (some? (.get ref)) (< i 50))
      (System/gc)
      (Thread/sleep 20)
      (recur (inc i))))
  (nil? (.get ref)))

(deftest a-key-compares-by-identity
  (let [a (java.util.HashMap.) b (java.util.HashMap.)]
    (is (= (pg-cache/identity-key a) (pg-cache/identity-key a)))
    (is (not= (pg-cache/identity-key a) (pg-cache/identity-key b))
        "equal values, different objects")
    (let [c (pg-cache/bounded-cache 4)]
      (.put c [(pg-cache/identity-key a) :x] 1)
      (is (= 1 (.get c [(pg-cache/identity-key a) :x])))
      (is (nil? (.get c [(pg-cache/identity-key b) :x]))))))

(deftest a-cached-entry-does-not-keep-its-key-alive
  (let [c (pg-cache/bounded-cache 16)
        ref (let [plan (Object.)]
              ;; the value refers to something big, as a cached plan's
              ;; artifacts can; it must go when the key's object does
              (.put c [(pg-cache/identity-key plan)] (byte-array 1024))
              (java.lang.ref.WeakReference. plan))]
    (testing "the entry alone does not hold the object"
      (is (collect! ref)))
    (testing "and the dead entry is swept on the next insertion"
      (is (= 1 (.size c)))
      (.put c [(pg-cache/identity-key (Object.))] :other)
      (is (= 1 (.size c))))))

(deftest the-bound-still-holds
  (let [c (pg-cache/bounded-cache 3)
        objs (vec (repeatedly 5 #(Object.)))]
    (doseq [o objs] (.put c [(pg-cache/identity-key o)] o))
    (is (= 3 (.size c)))
    (is (= (last objs) (.get c [(pg-cache/identity-key (last objs))])))
    (is (nil? (.get c [(pg-cache/identity-key (first objs))])))))
