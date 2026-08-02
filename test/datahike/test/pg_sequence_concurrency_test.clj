(ns datahike.test.pg-sequence-concurrency-test
  "Regression test for the sequence read-modify-write race in
   `handle-nextval`. Before the fix:

     T1 reads :__seq__/value=5
     T2 reads :__seq__/value=5
     T1 writes 6 → returns 6
     T2 writes 6 → returns 6     ; duplicate

   Two clients got the same nextval, and their downstream INSERTs
   into a `:db.unique/identity` PK column collide on `23505
   unique_violation`.

   The fix uses an optimistic `:db/cas`-retry loop: each iteration
   reads the current `:__seq__/value`, computes the new one, and
   submits `[[:db/cas eid :__seq__/value curr new]]`. Datahike's
   transactor serialises CAS submissions per conn, so the second
   submitter's old-val no longer matches the live state and the CAS
   raises `:transact/cas`. The retry re-reads with backoff and
   commits cleanly. The naive read-then-`d/transact` pattern can't
   detect this — by the time the second `d/transact` lands, the
   batch commit has already overwritten its tx-report's `:db-after`
   with the final post-batch db, so reading the assigned value back
   gives the wrong number for both callers.

   This test stresses that property: N threads × M nextval calls,
   assert the union of returned values has size N×M (no duplicates)
   and exactly covers the contiguous range PG semantics dictate."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(def ^:dynamic *conn* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*conn* conn]
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- nextval-values
  "Run `(SELECT nextval(seq-name))` n-calls times across n-threads
   parallel handlers. Returns the seq of all returned values."
  [seq-name n-threads n-calls]
  (let [handlers (repeatedly n-threads #(pg/make-query-handler *conn*))
        sql (str "SELECT nextval('" seq-name "')")
        worker (fn [^PgWireServer$QueryHandler h]
                 (vec (for [_ (range n-calls)]
                        (let [^PgWireServer$QueryResult r (.execute h sql)]
                          (when (.error r)
                            (throw (ex-info (.error r) {})))
                          ;; rows is Iterable<String[]>; each row is
                          ;; one column for nextval.
                          (Long/parseLong (aget ^"[Ljava.lang.String;"
                                           (first (.rows r)) 0))))))
        futures (mapv #(future (worker %)) handlers)
        results (mapv deref futures)]
    (reduce into [] results)))

(deftest nextval-no-duplicates-under-concurrency
  ;; Stress: 8 threads × 100 calls = 800 nextval invocations against one
  ;; sequence. Pre-fix this reliably surfaces duplicates within ~10 runs;
  ;; post-fix the set is exactly [1 .. 800] every time.
  (let [setup (pg/make-query-handler *conn*)]
    (let [^PgWireServer$QueryResult r (.execute setup "CREATE SEQUENCE s START WITH 1 INCREMENT BY 1")]
      (is (nil? (.error r)) (str "create sequence failed: " (.error r)))))
  (let [n-threads 8
        n-calls 100
        total (* n-threads n-calls)
        values (nextval-values "s" n-threads n-calls)]
    (is (= total (count values)) "got every call's return")
    (is (= total (count (distinct values)))
        (str "duplicates detected; got " (count (distinct values))
             " distinct values out of " total " calls"))
    (is (= (set (range 1 (inc total))) (set values))
        "values cover the contiguous range [1 … N×M] with no gaps")))

(deftest nextval-with-increment-by-step-no-duplicates
  ;; Same shape, but with INCREMENT BY 5. Starting at 0 needs an explicit
  ;; MINVALUE 0: an ascending sequence defaults to MINVALUE 1, and PG
  ;; rejects `START WITH 0` against it with 22023 "START value (0) cannot
  ;; be less than MINVALUE (1)" (verified against PostgreSQL 17.10). The
  ;; first nextval then returns 0 and each call advances by 5, giving a
  ;; contiguous [0, 5, 10, … (total-1)x5].
  (let [setup (pg/make-query-handler *conn*)]
    (let [^PgWireServer$QueryResult r (.execute setup "CREATE SEQUENCE s5 START WITH 0 MINVALUE 0 INCREMENT BY 5")]
      (is (nil? (.error r)))))
  (let [n-threads 4
        n-calls 25
        total (* n-threads n-calls)
        values (nextval-values "s5" n-threads n-calls)]
    (is (= total (count values)))
    (is (= total (count (distinct values)))
        "duplicates under INCREMENT BY 5")
    (is (= (set (range 0 (* 5 total) 5)) (set values))
        "values cover the contiguous range [0, 5, … (total-1)×5]")))

(deftest nextval-survives-rollback
  ;; PG semantics: a nextval advance is NEVER rolled back. After
  ;; ROLLBACK, the seq state reflects the bumped value, not the
  ;; pre-BEGIN value.
  (let [h (pg/make-query-handler *conn*)
        run! (fn [sql]
               (let [^PgWireServer$QueryResult r (.execute h sql)]
                 (when (.error r) (throw (ex-info (.error r) {})))
                 r))]
    (run! "CREATE SEQUENCE s_rb START WITH 1 INCREMENT BY 1")
    (let [pre  (Long/parseLong (aget ^"[Ljava.lang.String;"
                                (first (.rows ^PgWireServer$QueryResult
                                        (run! "SELECT nextval('s_rb')"))) 0))]
      (run! "BEGIN")
      (let [in-tx (Long/parseLong (aget ^"[Ljava.lang.String;"
                                   (first (.rows ^PgWireServer$QueryResult
                                           (run! "SELECT nextval('s_rb')"))) 0))]
        (run! "ROLLBACK")
        (let [post (Long/parseLong (aget ^"[Ljava.lang.String;"
                                    (first (.rows ^PgWireServer$QueryResult
                                            (run! "SELECT nextval('s_rb')"))) 0))]
          (is (= 1 pre))
          (is (= 2 in-tx))
          (is (= 3 post)
              "nextval advanced by ROLLBACK is preserved (PG semantics)"))))))

;; ============================================================================
;; CREATE SEQUENCE duplicate semantics (issue #15) — not a concurrency
;; property, but the handler fixture here is the natural home for
;; sequence-DDL execution tests.
;; ============================================================================

(deftest duplicate-create-sequence-raises-42p07
  ;; PG: re-running CREATE SEQUENCE on an existing sequence raises 42P07
  ;; duplicate_table (sequences are relations). Pre-fix this silently
  ;; re-transacted the init entity, RESETTING the live counter.
  (let [h (pg/make-query-handler *conn*)
        run! (fn [sql] (.execute h sql))]
    (is (nil? (.error ^PgWireServer$QueryResult (run! "CREATE SEQUENCE s_dup START WITH 1 INCREMENT BY 1"))))
    (is (= "1" (aget ^"[Ljava.lang.String;"
                (first (.rows ^PgWireServer$QueryResult (run! "SELECT nextval('s_dup')"))) 0)))
    (let [^PgWireServer$QueryResult dup (run! "CREATE SEQUENCE s_dup START WITH 1 INCREMENT BY 1")]
      (is (some? (.error dup)))
      (is (= "42P07" (.sqlstate dup)))
      (is (re-find #"relation \"s_dup\" already exists" (.error dup))))
    ;; The failed CREATE must not have touched the counter.
    (is (= "2" (aget ^"[Ljava.lang.String;"
                (first (.rows ^PgWireServer$QueryResult (run! "SELECT nextval('s_dup')"))) 0)))))

(deftest create-sequence-if-not-exists-noop-preserves-counter
  ;; PG: CREATE SEQUENCE IF NOT EXISTS on an existing sequence emits a
  ;; notice and completes as a no-op — the counter is NOT reset, even
  ;; when the retry carries a different START WITH.
  (let [h (pg/make-query-handler *conn*)
        run! (fn [sql] (.execute h sql))]
    ;; Fresh IF NOT EXISTS create works like a plain create.
    (let [^PgWireServer$QueryResult r (run! "CREATE SEQUENCE IF NOT EXISTS s_ine START WITH 1 INCREMENT BY 1")]
      (is (nil? (.error r)) (str "create failed: " (.error r)))
      (is (= "CREATE SEQUENCE" (.commandTag r))))
    (is (= "1" (aget ^"[Ljava.lang.String;"
                (first (.rows ^PgWireServer$QueryResult (run! "SELECT nextval('s_ine')"))) 0)))
    ;; Idempotent retry: success tag, no counter reset.
    (let [^PgWireServer$QueryResult r (run! "CREATE SEQUENCE IF NOT EXISTS s_ine START WITH 100 INCREMENT BY 1")]
      (is (nil? (.error r)))
      (is (= "CREATE SEQUENCE" (.commandTag r))))
    (is (= "2" (aget ^"[Ljava.lang.String;"
                (first (.rows ^PgWireServer$QueryResult (run! "SELECT nextval('s_ine')"))) 0))
        "counter preserved across IF NOT EXISTS retry (no reset to 100)")))
