(ns datahike.test.pg-comparator-test
  "ONE comparator and ONE equality key, which is what Phase 1.4 of the
   consolidation plan asks for.

   Four comparators had drifted apart -- `order-cmp` (ORDER BY),
   `sql-order-cmp` (the `<` family), `akey-compare` (an aggregate's own
   ORDER BY) and a verbatim copy of `null-safe-order-cmp` in `stmt.clj`
   -- each with its own `:else (compare a b)`. The tests below pin the
   cases where that disagreement was observable, every one of them
   checked against a real PostgreSQL 17.10 and its answer quoted."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.pg.sql.fns :as fns])
  (:import [java.time LocalDate LocalDateTime]
           [java.util UUID]))

(def ^:private hi
  "A uuid whose most significant bit is SET, so a signed compare puts it
   below everything."
  (UUID/fromString "ff000000-0000-0000-0000-000000000000"))

(def ^:private lo (UUID/fromString "00000000-0000-0000-0000-000000000001"))
(def ^:private mid (UUID/fromString "7f000000-0000-0000-0000-000000000000"))

(deftest test-uuid-is-ordered-unsigned
  (testing "`uuid_cmp` (uuid.c) compares 16 UNSIGNED bytes, while
            java.util.UUID.compareTo compares its two longs SIGNED"
    ;; Oracle: SELECT 'ff..'::uuid > '7f..'::uuid  =>  t
    (is (pos? (fns/order-cmp hi mid)))
    (is (pos? (fns/order-cmp hi lo)))
    (is (neg? (fns/order-cmp lo mid)))
    ;; and java's own answer is the opposite, which is the whole point
    (is (neg? (.compareTo hi mid))))

  (testing "ORDER BY and the `<` family give the SAME answer -- this arm
            lived only in `sql-order-cmp`, so `WHERE a < b` and
            `ORDER BY a` disagreed for exactly these values"
    (doseq [[a b] [[hi mid] [hi lo] [lo mid] [mid hi]]]
      (is (= (Integer/signum (fns/order-cmp a b))
             (Integer/signum (@#'fns/sql-order-cmp a b)))
          (str a " vs " b))))

  (testing "sorting agrees with the oracle's ORDER BY"
    ;; Oracle: 00000000-…-01, 7f00…, ff00…
    (is (= [lo mid hi] (sort fns/order-cmp [hi lo mid])))))

(deftest test-value-key-families
  (testing "the four families where `clojure.core/=` disagrees with
            PostgreSQL's opclass equality, all four oracle-checked"
    ;; Oracle: GROUP BY over two 'NaN'::float8  =>  1 group
    (is (= (fns/value-key Double/NaN) (fns/value-key Double/NaN)))
    (is (not= (fns/value-key Double/NaN) (fns/value-key 1.0)))

    ;; Oracle: GROUP BY over two '\x01'::bytea  =>  1 group
    (is (= (fns/value-key (byte-array [1 2])) (fns/value-key (byte-array [1 2]))))
    (is (not= (fns/value-key (byte-array [1 2])) (fns/value-key (byte-array [3]))))

    ;; Oracle: 1.0::numeric = 1.00::numeric  =>  t, and ::text keeps the
    ;; scale, so the equality is COARSER than the text
    (is (= (fns/value-key (bigdec "1.0")) (fns/value-key (bigdec "1.00"))))
    (is (not= (fns/value-key (bigdec "1.0")) (fns/value-key (bigdec "1.1")))))

  (testing "and leaves an ordinary value untouched"
    (doseq [v [1 "s" :k 1.5 true]]
      (is (= v (fns/value-key v))))))

(deftest test-aggregate-order-by-uses-one-comparator
  (testing "an aggregate's own ORDER BY was the one sort in the system
            with its own `:else (compare a b)`: uuids sorted SIGNED"
    ;; Oracle: string_agg(v::text, ',' ORDER BY v) over the three uuids
    ;;   =>  00000000-…-01,7f00…,ff00…
    (is (= [lo mid hi] (sort @#'fns/akey-compare [hi lo mid]))))

  (testing "and a bytea sort key threw, because a byte array is not
            Comparable at all"
    ;; Oracle: string_agg(encode(v,'hex'), ',' ORDER BY v)  =>  01,7f,ff
    (is (= [[1] [127] [-1]]
           (mapv vec (sort @#'fns/akey-compare
                           [(byte-array [-1]) (byte-array [1]) (byte-array [127])])))))

  (testing "NULLs still sort last for ASC, and `:__null__` counts as one"
    (is (= [1 2 nil] (sort @#'fns/akey-compare [2 nil 1])))
    (is (= [1 2 :__null__] (sort @#'fns/akey-compare [2 :__null__ 1])))
    (is (zero? (@#'fns/akey-compare :__null__ nil)))))

(deftest test-mixed-temporal-carriers-compare
  (testing "a `date` column carries a LocalDate and a timestamp a
            java.util.Date or LocalDateTime; the three are not mutually
            Comparable, and TWO functions named `temporal-instant` --
            the second shadowing the first -- meant `order-cmp` got the
            narrow one. A CHECK constraint comparing a LocalDate against
            a java.util.Date then answered 42883 instead of 23514."
    (let [ld   (LocalDate/of 2030 1 1)
          ldt  (LocalDateTime/of 2030 1 1 0 0)
          date (java.util.Date/from (.toInstant (.atStartOfDay ld) java.time.ZoneOffset/UTC))]
      (is (zero? (fns/order-cmp ld date)))
      (is (zero? (fns/order-cmp date ld)))
      (is (zero? (fns/order-cmp ldt date)))
      (is (neg? (fns/order-cmp (LocalDate/of 2029 1 1) date)))
      (is (pos? (fns/order-cmp (LocalDate/of 2031 1 1) date))))))

(deftest test-nan-ordering
  (testing "PostgreSQL sorts NaN GREATER than every non-NaN and equal to
            itself (float8_cmp_internal), where `compare` reports it
            equal to EVERYTHING and leaves a sort silently untouched"
    (is (zero? (fns/order-cmp Double/NaN Double/NaN)))
    (is (pos? (fns/order-cmp Double/NaN 1.0)))
    (is (neg? (fns/order-cmp 1.0 Double/NaN)))
    ;; Oracle: SELECT string_agg(v::text, ',' ORDER BY v) over NaN,1,2
    ;;   =>  1,2,NaN
    (is (= [0.5 1.0 2.0] (take 3 (sort fns/order-cmp [1.0 Double/NaN 0.5 2.0]))))
    (is (Double/isNaN (last (sort fns/order-cmp [1.0 Double/NaN 0.5 2.0]))))))
