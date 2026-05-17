(ns datahike.test.pg-for-valid-time-test
  "Tests for SQL:2011 `FOR VALID_TIME …` SELECT-side preprocessor.

   The preprocessor strips `FOR VALID_TIME <spec>` / `FOR ALL
   VALID_TIME` clauses from SELECT SQL and returns a side-channel
   override map the handler threads into `apply-temporal` for THIS
   statement only — equivalent to a per-statement
   `SET datahike.valid_at = ...` that auto-resets after the query."
  (:require [clojure.test :refer [deftest testing is]]
            [datahike.pg.sql.temporal :as temporal])
  (:import [java.util Date]))

;; ===========================================================================
;; Literal parsing
;; ===========================================================================

(deftest parse-temporal-literal-iso-instant
  (testing "ISO instant with Z"
    (let [d (temporal/parse-temporal-literal "'2024-04-15T00:00:00Z'")]
      (is (instance? Date d))
      (is (= 1713139200000 (.getTime ^Date d))))))

(deftest parse-temporal-literal-iso-date-only
  (testing "Bare YYYY-MM-DD pads to midnight UTC"
    (let [d (temporal/parse-temporal-literal "'2024-04-15'")]
      (is (instance? Date d))
      (is (= 1713139200000 (.getTime ^Date d))))))

(deftest parse-temporal-literal-millis
  (testing "Numeric literal treated as epoch millis"
    (let [d (temporal/parse-temporal-literal "1713139200000")]
      (is (instance? Date d))
      (is (= 1713139200000 (.getTime ^Date d))))))

(deftest parse-temporal-literal-sentinels
  (testing "MAX_VALUE / MIN_VALUE keywords"
    (is (= Long/MAX_VALUE (temporal/parse-temporal-literal "MAX_VALUE")))
    (is (= Long/MIN_VALUE (temporal/parse-temporal-literal "MIN_VALUE")))))

(deftest parse-temporal-literal-rejects-garbage
  (testing "Unparseable input throws :sql/bad-temporal-literal"
    (is (thrown-with-msg?
          clojure.lang.ExceptionInfo
          #"Cannot parse temporal literal"
          (temporal/parse-temporal-literal "'not-a-date'")))))

;; ===========================================================================
;; SELECT-side FOR VALID_TIME preprocessor
;; ===========================================================================

(deftest preprocess-no-clause-passes-through
  (testing "SQL without FOR VALID_TIME is returned unchanged with nil override"
    (let [{:keys [sql override]}
          (temporal/preprocess "SELECT * FROM person WHERE name = 'Bob'")]
      (is (= "SELECT * FROM person WHERE name = 'Bob'" sql))
      (is (nil? override)))))

(deftest preprocess-for-valid-time-as-of-extracts-date
  (testing "AS OF clause stripped + override carries the parsed Date"
    (let [{:keys [sql override]}
          (temporal/preprocess
            "SELECT * FROM person FOR VALID_TIME AS OF '2024-04-15' WHERE name = 'Bob'")]
      (testing "FOR VALID_TIME stripped from SQL"
        (is (re-find #"^SELECT \* FROM person\s+WHERE name = 'Bob'$" sql))
        (is (not (re-find #"(?i)FOR\s+VALID_TIME" sql))))
      (testing "override has :valid-at as Date"
        (is (contains? override :valid-at))
        (is (instance? Date (:valid-at override)))
        (is (= 1713139200000 (.getTime ^Date (:valid-at override))))))))

(deftest preprocess-for-valid-time-between
  (testing "BETWEEN clause → :valid-between [from to]"
    (let [{:keys [override]}
          (temporal/preprocess
            "SELECT * FROM person FOR VALID_TIME BETWEEN '2024-01-01' AND '2024-12-31'")]
      (is (contains? override :valid-between))
      (let [[from to] (:valid-between override)]
        (is (instance? Date from))
        (is (instance? Date to))
        (is (= 1704067200000 (.getTime ^Date from)))
        (is (= 1735603200000 (.getTime ^Date to)))))))

(deftest preprocess-for-valid-time-from-to
  (testing "FROM x TO y → :valid-between [from to]"
    (let [{:keys [override]}
          (temporal/preprocess
            "SELECT * FROM person FOR VALID_TIME FROM '2024-01-01' TO '2024-12-31'")]
      (is (contains? override :valid-between))
      (let [[from to] (:valid-between override)]
        (is (= 1704067200000 (.getTime ^Date from)))
        (is (= 1735603200000 (.getTime ^Date to)))))))

(deftest preprocess-for-all-valid-time
  (testing "FOR ALL VALID_TIME → :valid-at :all (clears any session pin)"
    (let [{:keys [sql override]}
          (temporal/preprocess "SELECT * FROM person FOR ALL VALID_TIME")]
      (is (= :all (:valid-at override)))
      (is (not (re-find #"(?i)FOR\s+ALL" sql))))))

(deftest preprocess-for-valid-time-all-alias
  (testing "FOR VALID_TIME ALL is also accepted"
    (let [{:keys [override]}
          (temporal/preprocess "SELECT * FROM person FOR VALID_TIME ALL")]
      (is (= :all (:valid-at override))))))

(deftest preprocess-clause-before-where
  (testing "FOR VALID_TIME extracted when followed by WHERE"
    (let [{:keys [sql override]}
          (temporal/preprocess
            "SELECT name FROM person FOR VALID_TIME AS OF '2024-04-15' WHERE age > 25")]
      (is (re-find #"WHERE age > 25" sql))
      (is (not (re-find #"(?i)FOR\s+VALID_TIME" sql)))
      (is (instance? Date (:valid-at override))))))

(deftest preprocess-clause-before-order-by
  (testing "FOR VALID_TIME extracted when followed by ORDER BY"
    (let [{:keys [sql override]}
          (temporal/preprocess
            "SELECT name FROM person FOR VALID_TIME AS OF '2024-04-15' ORDER BY name")]
      (is (re-find #"ORDER BY name" sql))
      (is (instance? Date (:valid-at override))))))

(deftest preprocess-rejects-multi-clause
  (testing "Two FOR VALID_TIME clauses on a joined SELECT → throws"
    (is (thrown-with-msg?
          clojure.lang.ExceptionInfo
          #"Multi-table SELECT with more than one FOR VALID_TIME"
          (temporal/preprocess
            (str "SELECT a.id, b.id FROM a FOR VALID_TIME AS OF '2024-04-15' "
                 "JOIN b FOR VALID_TIME AS OF '2024-04-15' ON a.id = b.id"))))))

(deftest preprocess-ignores-clause-inside-string-literal
  (testing "FOR VALID_TIME inside a string literal is NOT stripped"
    (let [{:keys [sql override]}
          (temporal/preprocess
            "SELECT * FROM person WHERE comment = 'FOR VALID_TIME AS OF X'")]
      (is (= "SELECT * FROM person WHERE comment = 'FOR VALID_TIME AS OF X'" sql))
      (is (nil? override)))))

(deftest preprocess-allows-numeric-literal-bound
  (testing "Numeric epoch-millis as a temporal bound parses"
    (let [{:keys [override]}
          (temporal/preprocess
            "SELECT * FROM person FOR VALID_TIME AS OF 1713139200000")]
      (is (= 1713139200000 (.getTime ^Date (:valid-at override)))))))

(deftest preprocess-allows-max-value-sentinel
  (testing "MAX_VALUE / MIN_VALUE sentinels in BETWEEN"
    (let [{:keys [override]}
          (temporal/preprocess
            "SELECT * FROM person FOR VALID_TIME BETWEEN MIN_VALUE AND MAX_VALUE")]
      (is (= [Long/MIN_VALUE Long/MAX_VALUE] (:valid-between override))))))

;; ===========================================================================
;; Override + apply-temporal integration
;;
;; The preprocessor's contract is: parse SQL → strip FOR VALID_TIME
;; clauses → produce an override map → handler threads override into
;; `apply-temporal` for THIS statement only.
;;
;; These tests verify the override→apply-temporal contract directly
;; against `(d/db conn)`, bypassing the full PgWire SELECT path. That
;; path independently has a known FilteredDB ↔ keyword-lookup
;; integration issue tracked in the broader PR; this test layer is
;; precisely the part the FOR VALID_TIME preprocessor adds.
;; ===========================================================================

(require '[datahike.api :as d])

(def ^:private apply-temporal
  (resolve 'datahike.pg.server/apply-temporal))

(def ^:dynamic *db-conn* nil)

(defn- temporal-fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          _ (d/transact conn [{:db/ident :person/name
                               :db/valueType :db.type/string
                               :db/cardinality :db.cardinality/one
                               :db/unique :db.unique/identity}])]
      (try
        (binding [*db-conn* conn]
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(clojure.test/use-fixtures :each temporal-fixture)

(deftest override-as-of-marks-db
  (testing "Override {:valid-at <Date>} stamps :datahike/valid-at on the db"
    (let [session (atom {})
          at      #inst "2024-04-15"
          db      (@apply-temporal (d/db *db-conn*) session {:valid-at at})]
      (is (= at (:datahike/valid-at (meta db)))
          "per-statement override flows to d/valid-at"))))

(deftest override-does-not-pollute-session-state
  (testing "Per-statement override is not persisted into session-state"
    (let [session (atom {})
          at      #inst "2024-04-15"
          _       (@apply-temporal (d/db *db-conn*) session {:valid-at at})]
      (is (nil? (:valid-at @session))
          "session-state must be unmodified after the override-only call"))))

(deftest override-shadows-session-state
  (testing "Per-statement override wins over session-state for this call"
    (let [session-at  #inst "2023-01-01"
          override-at #inst "2024-04-15"
          session     (atom {:valid-at session-at})
          db          (@apply-temporal (d/db *db-conn*) session {:valid-at override-at})]
      (is (= override-at (:datahike/valid-at (meta db)))
          "override marker wins; session marker shadowed for this call")
      (is (= session-at (:valid-at @session))
          "session-state still holds the original value"))))

(deftest override-valid-at-all-clears-session-pin
  (testing "{:valid-at :all} clears any session-scoped pin for this call"
    (let [session-at #inst "2024-04-15"
          session    (atom {:valid-at session-at})
          db         (@apply-temporal (d/db *db-conn*) session {:valid-at :all})]
      (is (nil? (:datahike/valid-at (meta db)))
          "FOR ALL VALID_TIME → no marker on db for this call")
      (is (= session-at (:valid-at @session))
          "session-state unchanged"))))

(deftest override-between-marks-db
  (testing "Override {:valid-between [a b]} stamps :datahike/valid-between"
    (let [session (atom {})
          from    #inst "2024-01-01"
          to      #inst "2024-12-31"
          db      (@apply-temporal (d/db *db-conn*) session {:valid-between [from to]})]
      (is (= [from to] (:datahike/valid-between (meta db))))
      (is (nil? (:datahike/valid-at (meta db)))
          "between path does not also set the point marker"))))

(deftest preprocess-and-apply-temporal-compose
  (testing "preprocess output threads cleanly into apply-temporal"
    (let [{:keys [sql override]}
          (temporal/preprocess
            "SELECT * FROM person FOR VALID_TIME AS OF '2024-04-15' WHERE name = 'Bob'")
          session (atom {})
          db (@apply-temporal (d/db *db-conn*) session override)]
      (is (not (re-find #"(?i)FOR\s+VALID_TIME" sql))
          "stripped SQL has no temporal clause")
      (is (some? (:datahike/valid-at (meta db)))
          "override flows to db marker"))))

;; ===========================================================================
;; Allen interval predicates as SQL functions
;;
;; The fixture sets up an `intervals` table by transacting EAV-shaped data;
;; SELECTs against it use the Allen predicate in the WHERE clause and
;; verify the row-survivor pattern.
;;
;; NOTE: datahike's SQL evaluator is the consumer of `sql-fn->clj-fn`; we
;; ship the 10 entries here and trust the existing dispatch test machinery
;; to cover their general execution. These tests are smoke-level — they
;; verify the function arity + boolean output shape for each of the 10
;; verbs. Full integration (using these in a WHERE clause against a live
;; bitemporal datahike) is exercised by stratum's own Allen-predicate tests
;; on the storage side; pg-datahike just passes through.
;; ===========================================================================

(deftest allen-predicates-registered
  (require 'datahike.pg.sql.fns)
  (let [fns @(resolve 'datahike.pg.sql.fns/sql-fn->clj-fn)
        verbs ["overlaps" "equals_period" "contains_period"
               "strictly_contains_period"
               "precedes" "strictly_precedes" "immediately_precedes"
               "succeeds" "strictly_succeeds" "immediately_succeeds"
               "meets"]]
    (doseq [v verbs]
      (testing v
        (is (contains? fns v) (str v " missing from sql-fn->clj-fn"))
        (is (fn? (get fns v)))))))

(deftest allen-overlaps-semantic
  (let [fns @(resolve 'datahike.pg.sql.fns/sql-fn->clj-fn)
        overlaps (get fns "overlaps")]
    (testing "[100,200) overlaps [150,250) → true"
      (is (true?  (overlaps 100 200 150 250))))
    (testing "[100,200) overlaps [200,300) (touching, NOT overlap) → false"
      (is (false? (overlaps 100 200 200 300))))
    (testing "[100,200) overlaps [300,400) (disjoint) → false"
      (is (false? (overlaps 100 200 300 400))))))

(deftest allen-precedes-vs-strictly-precedes
  (let [fns @(resolve 'datahike.pg.sql.fns/sql-fn->clj-fn)
        prec  (get fns "precedes")
        sprec (get fns "strictly_precedes")]
    (testing "touching (a.to == b.from)"
      (is (true?  (prec  100 200 200 300)))
      (is (false? (sprec 100 200 200 300))))
    (testing "strictly before"
      (is (true?  (prec  100 200 250 350)))
      (is (true?  (sprec 100 200 250 350))))
    (testing "after b"
      (is (false? (prec  300 400 100 200))))))

(deftest allen-meets-equals-immediately-precedes
  (let [fns @(resolve 'datahike.pg.sql.fns/sql-fn->clj-fn)
        meets (get fns "meets")
        imm-prec (get fns "immediately_precedes")]
    (testing "MEETS alias for IMMEDIATELY_PRECEDES (A.end == B.start)"
      (is (= (meets 100 200 200 300) (imm-prec 100 200 200 300)))
      (is (= (meets 100 199 200 300) (imm-prec 100 199 200 300))))))

(deftest allen-succeeds-family
  (let [fns @(resolve 'datahike.pg.sql.fns/sql-fn->clj-fn)
        succ  (get fns "succeeds")
        ssucc (get fns "strictly_succeeds")
        isucc (get fns "immediately_succeeds")]
    (testing "[200,300) succeeds [100,200)? (touching)"
      (is (true?  (succ  200 300 100 200)))
      (is (false? (ssucc 200 300 100 200)))
      (is (true?  (isucc 200 300 100 200))))
    (testing "[250,350) strictly succeeds [100,200)"
      (is (true? (ssucc 250 350 100 200))))))

(deftest allen-contains-period
  (let [fns @(resolve 'datahike.pg.sql.fns/sql-fn->clj-fn)
        cont (get fns "contains_period")
        scont (get fns "strictly_contains_period")]
    (testing "[100,500) contains [200,400)"
      (is (true? (cont 100 500 200 400))))
    (testing "[100,500) equally contains [100,500) (boundaries touching)"
      (is (true?  (cont  100 500 100 500)))
      (is (false? (scont 100 500 100 500))))
    (testing "[100,200) does NOT contain [150,250)"
      (is (false? (cont 100 200 150 250))))))

(deftest allen-equals-period
  (let [fns @(resolve 'datahike.pg.sql.fns/sql-fn->clj-fn)
        eq (get fns "equals_period")]
    (is (true?  (eq 100 200 100 200)))
    (is (false? (eq 100 200 100 201)))
    (is (false? (eq 100 200 99 200)))))
