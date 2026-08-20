(ns datahike.pg.prng
  "PostgreSQL's pseudo-random generator, ported from src/common/pg_prng.c.

   Ported rather than substituted for java.util.Random because it makes
   `random()` TESTABLE: `SETSEED(0.5)` followed by a fixed number of
   `random()` calls produces one exact sequence, so the differential
   against a real PostgreSQL is bit-for-bit rather than a smoke test
   that can only check the range.

   xoroshiro128** with a splitmix64 seeder. All arithmetic is on Java
   longs, which are the same 64 bits as PostgreSQL's uint64 -- only the
   comparisons and the shift right differ, so every right shift here is
   the UNSIGNED one."
  (:import [java.util.concurrent.atomic AtomicReference]))

(set! *warn-on-reflection* true)

(defn- rotl ^long [^long x ^long bits]
  (bit-or (bit-shift-left x bits) (unsigned-bit-shift-right x (- 64 bits))))

(defn- splitmix64
  "Returns `[value next-seed]` -- pg_prng.c's splitmix64 mutates its
   argument, which here has to travel back out."
  [^long seed]
  (let [s (unchecked-add seed (unchecked-long 0x9E3779B97f4A7C15))
        v (unchecked-multiply (bit-xor s (unsigned-bit-shift-right s 30))
                              (unchecked-long 0xBF58476D1CE4E5B9))
        v (unchecked-multiply (bit-xor v (unsigned-bit-shift-right v 27))
                              (unchecked-long 0x94D049BB133111EB))]
    [(bit-xor v (unsigned-bit-shift-right v 31)) s]))

(defn seed-state
  "pg_prng_seed: two splitmix64 draws from the seed. The all-zero state
   is invalid for xoroshiro, and PostgreSQL substitutes a fixed pair."
  [^long seed]
  (let [[s0 seed'] (splitmix64 seed)
        [s1 _] (splitmix64 seed')]
    (if (and (zero? s0) (zero? s1))
      [(unchecked-long 0x5A5A5A5A5A5A5A5A) (unchecked-long 0x5A5A5A5A5A5A5A5A)]
      [s0 s1])))

(defn fseed-state
  "pg_prng_fseed: scale the [-1,1] argument by 2^52-1 and truncate toward
   zero, exactly as the C cast to int64 does."
  [^double fseed]
  (seed-state (long (* (double (dec (bit-shift-left 1 52))) fseed))))

(defn next-u64
  "One xoroshiro128** step. Returns `[value new-state]`."
  [[^long s0 ^long s1]]
  (let [sx (bit-xor s1 s0)
        val (unchecked-multiply (rotl (unchecked-multiply s0 5) 7) 9)]
    [val [(bit-xor (rotl s0 24) sx (bit-shift-left sx 16))
          (rotl sx 37)]]))

(defn next-double
  "pg_prng_double: the top 52 bits scaled into [0,1). Returns
   `[value new-state]`."
  [state]
  (let [[v st'] (next-u64 state)]
    [(Math/scalb ^double (double (unsigned-bit-shift-right v 12)) (int -52)) st']))

(defn make-session-state
  "A per-session PRNG cell. PostgreSQL seeds from the clock and the
   backend PID until SETSEED is called."
  []
  (AtomicReference. (seed-state (bit-xor (System/nanoTime)
                                         (long (.hashCode (Thread/currentThread)))))))

(defn draw-double!
  "Advance `cell` and return the next double in [0,1)."
  ^double [^AtomicReference cell]
  (loop []
    (let [cur (.get cell)
          [v nxt] (next-double cur)]
      (if (.compareAndSet cell cur nxt) v (recur)))))

(defn set-seed!
  [^AtomicReference cell ^double fseed]
  (.set cell (fseed-state fseed))
  nil)
