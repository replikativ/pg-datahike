(ns datahike.test.pg-commit-ring-test
  "Coverage of the recent-commit write-set ring.

   The ring is what lets a COMMIT test row-level overlap in
   O(concurrent writes) instead of scanning the whole database. Its
   answer matters in both directions: a wrong ::gap costs a
   full-database scan (about 1 s at 3M datoms), and a wrongly CONFIRMED
   window would miss a concurrent write set and let a conflicting
   transaction commit. Nothing covered this logic before.

   Coverage is established by chaining each entry's `:prev-max-tx`, not
   by counting entries against the max-tx difference. The counting
   version assumed max-tx advances by exactly one per transact -- true
   today, false under a Datomic-style shared `t` counter where
   `tx = (3<<42)|t`."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.pg.server :as pg]))

(def ^:private ring #'pg/recent-commit-writes)
(def ^:private record! #'pg/record-commit-writes!)
(def ^:private coverage #'pg/ring-write-eas)

(defn- clear! [f]
  (reset! @ring clojure.lang.PersistentQueue/EMPTY)
  (f))

(use-fixtures :each clear!)

(defn- commits!
  "Record a chain of commits for `db-key`: each [prev max eas]."
  [db-key & triples]
  (doseq [[prev mx eas] triples] (@record! db-key prev mx eas)))

(deftest a-dense-window-is-covered
  (commits! :db [5 6 #{[1 :a]}] [6 7 #{[2 :b]}])
  (is (= #{[1 :a] [2 :b]} (@coverage :db 5 7))))

(deftest a-window-whose-max-tx-jumps-is-still-covered
  ;; The reason for chaining. Under a shared t counter each transact
  ;; consumes several t values, so max-tx advances by more than one.
  ;; The count-based test wanted 245 entries here and answered ::gap,
  ;; which sends every COMMIT to the whole-database scan.
  (commits! :db [5 100 #{[1 :a]}] [100 250 #{[2 :b]}])
  (is (= #{[1 :a] [2 :b]} (@coverage :db 5 250))
      "coverage must not depend on how far max-tx advanced")
  (testing "and a single jumping commit is covered too"
    (clear! (fn []))
    (commits! :db [5 4096 #{[3 :c]}])
    (is (= #{[3 :c]} (@coverage :db 5 4096)))))

(deftest a-broken-chain-is-a-gap
  (testing "the oldest entry does not start at begin -- an evicted commit"
    (commits! :db [6 7 #{[1 :a]}])
    (is (= ::pg/gap (@coverage :db 5 7))))
  (testing "a hole in the middle -- a commit that bypassed the ring"
    (clear! (fn []))
    (commits! :db [5 6 #{[1 :a]}] [8 9 #{[2 :b]}])
    (is (= ::pg/gap (@coverage :db 5 9))))
  (testing "the newest entry does not reach current"
    (clear! (fn []))
    (commits! :db [5 6 #{[1 :a]}])
    (is (= ::pg/gap (@coverage :db 5 9))))
  (testing "the window advanced but nothing was recorded at all"
    (clear! (fn []))
    (is (= ::pg/gap (@coverage :db 5 9)))))

(deftest an-opaque-entry-is-a-gap
  (commits! :db [5 6 #{[1 :a]}] [6 7 ::pg/opaque])
  (is (= ::pg/gap (@coverage :db 5 7))))

(deftest a-window-of-only-sequence-reservations-says-so
  (commits! :db [5 6 ::pg/sequence-only] [6 7 ::pg/sequence-only])
  (is (= ::pg/sequence-only (@coverage :db 5 7)))
  (testing "mixed with a real write, the write set wins"
    (clear! (fn []))
    (commits! :db [5 6 ::pg/sequence-only] [6 7 #{[2 :b]}])
    (is (= #{[2 :b]} (@coverage :db 5 7)))))

(deftest an-unadvanced-window-is-not-a-gap
  ;; begin == current: nothing committed, so there is nothing to scan.
  (is (= ::pg/sequence-only (@coverage :db 5 5))))

(deftest another-databases-commits-do-not-count
  ;; The ring is shared; an unkeyed one could mistake another database's
  ;; commit for coverage of this one's window.
  (commits! :other [5 6 #{[1 :a]}] [6 7 #{[2 :b]}])
  (is (= ::pg/gap (@coverage :db 5 7)))
  (testing "and our own chain is unaffected by theirs"
    (commits! :db [5 6 #{[9 :z]}] [6 7 #{[8 :y]}])
    (is (= #{[9 :z] [8 :y]} (@coverage :db 5 7)))))
