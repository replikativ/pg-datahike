(ns datahike.test.pg-sequence-ddl-test
  "CREATE / ALTER SEQUENCE option parsing, defaults and validation
   (issue #21).

   JSqlParser's CreateSequence grammar covers only a subset of PG's
   option list — `INCREMENT BY n` but not `INCREMENT n`, no `AS type`, no
   `IF NOT EXISTS`, no `NO MINVALUE`, no signed values — and has no
   AlterSequence branch at all. Worse, the options that DID parse were
   then recovered by regex over JSqlParser's re-rendered SQL
   (`increment\\s+by\\s+(\\d+)`), which cannot see a negative increment
   and silently dropped MINVALUE / MAXVALUE / CACHE / CYCLE.

   Sequence DDL is therefore token-classified in full. Every default and
   every error message below was captured from PostgreSQL 17.10; the
   validation order mirrors init_params (sequence.c:1260) because a
   statement with more than one problem must report the same one PG
   reports."
  (:require [clojure.test :refer [deftest testing is]]
            [datahike.pg.errors :as errors]
            [datahike.pg.sql.classify :as cls]
            [datahike.pg.sql.ddl :as ddl]))

(defn- opts-of [sql]
  (let [{:keys [kind seq-opts]} (cls/classify sql)]
    (when (#{:create-sequence :alter-sequence} kind) seq-opts)))

(defn- params-of [sql]
  (ddl/sequence-params (opts-of sql) {}))

(defn- error-of [sql]
  (try (params-of sql) nil
       (catch Exception e (let [[code msg _] (errors/classify-exception e)] [code msg]))))

;; ============================================================================
;; Option parsing — the shapes JSqlParser could not read
;; ============================================================================

(deftest test-optional-noise-words
  (testing "BY after INCREMENT and WITH after START/RESTART are optional"
    ;; `INCREMENT 20` is the statement in the issue report.
    (is (= [[:increment 20] [:start 400]]
           (opts-of "CREATE SEQUENCE s INCREMENT 20 START WITH 400")))
    (is (= [[:increment 20] [:start 400]]
           (opts-of "CREATE SEQUENCE s INCREMENT BY 20 START 400")))
    (is (= [[:restart 7]] (opts-of "ALTER SEQUENCE s RESTART WITH 7")))
    (is (= [[:restart 7]] (opts-of "ALTER SEQUENCE s RESTART 7")))))

(deftest test-signed-values
  (testing "every numeric option accepts an explicit sign"
    (is (= [[:increment -1]] (opts-of "CREATE SEQUENCE s INCREMENT -1")))
    (is (= [[:increment 1]]  (opts-of "CREATE SEQUENCE s INCREMENT +1")))
    (is (= [[:minvalue -100] [:maxvalue -1]]
           (opts-of "CREATE SEQUENCE s MINVALUE -100 MAXVALUE -1")))))

(deftest test-bare-restart-needs-lookahead
  (testing "RESTART is the only option whose value is optional"
    (is (= [[:restart :default]] (opts-of "ALTER SEQUENCE s RESTART")))
    (is (= [[:restart :default] [:increment 5]]
           (opts-of "ALTER SEQUENCE s RESTART INCREMENT 5"))
        "a following option keyword must not be eaten as RESTART's value")))

(deftest test-full-option-list
  (testing "AS / CACHE / CYCLE / OWNED BY all parse"
    (is (= [[:as "bigint"] [:increment 2] [:minvalue 1] [:maxvalue 100]
            [:cache 1] [:cycle true]]
           (opts-of "CREATE SEQUENCE s AS bigint INCREMENT BY 2 MINVALUE 1 MAXVALUE 100 CACHE 1 CYCLE"))))
  (testing "NO MINVALUE / NO MAXVALUE / NO CYCLE"
    (is (= [[:minvalue :none] [:maxvalue :none] [:cycle false]]
           (opts-of "CREATE SEQUENCE s NO MINVALUE NO MAXVALUE NO CYCLE"))))
  (testing "IF NOT EXISTS and an empty option list"
    (is (true? (:if-not-exists? (cls/classify "CREATE SEQUENCE IF NOT EXISTS s"))))
    (is (= [] (opts-of "CREATE SEQUENCE s")))))

(deftest test-alter-requires-an-option
  (testing "ALTER SEQUENCE takes SeqOptList (>= 1), CREATE takes OptSeqOptList"
    (is (= :create-sequence (:kind (cls/classify "CREATE SEQUENCE s"))))
    (is (= :generic-sql (:kind (cls/classify "ALTER SEQUENCE s"))))))

;; ============================================================================
;; Defaults
;; ============================================================================

(deftest test-defaults-ascending
  (testing "an unqualified sequence is bigint, 1..2^63-1, start 1"
    (is (= {:increment 1 :minvalue 1 :maxvalue 9223372036854775807
            :start 1 :cache 1 :cycle? false}
           (select-keys (params-of "CREATE SEQUENCE s")
                        [:increment :minvalue :maxvalue :start :cache :cycle?])))))

(deftest test-defaults-descending
  (testing "INCREMENT -1 flips the defaults: max -1, start -1, min type-min"
    ;; The easy thing to get wrong is defaulting max to the type max and
    ;; start to min regardless of direction.
    (is (= {:increment -1 :minvalue -9223372036854775808 :maxvalue -1 :start -1}
           (select-keys (params-of "CREATE SEQUENCE s INCREMENT -1")
                        [:increment :minvalue :maxvalue :start])))))

(deftest test-defaults-follow-as-type
  (testing "AS smallint / integer narrow the bounds"
    (is (= [1 32767] ((juxt :minvalue :maxvalue)
                      (params-of "CREATE SEQUENCE s AS smallint"))))
    (is (= [-2147483648 -1] ((juxt :minvalue :maxvalue)
                             (params-of "CREATE SEQUENCE s AS integer INCREMENT -1"))))))

(deftest test-no-minvalue-means-default-not-unbounded
  (testing "NO MINVALUE recomputes the default for the type and direction"
    (is (= 1 (:minvalue (params-of "CREATE SEQUENCE s NO MINVALUE"))))
    (is (= -9223372036854775808
           (:minvalue (params-of "CREATE SEQUENCE s INCREMENT -1 NO MINVALUE"))))))

;; ============================================================================
;; Validation — PG's messages, verbatim
;; ============================================================================

(deftest test-validation-errors
  (testing "22023 invalid_parameter_value"
    (is (= ["22023" "INCREMENT must not be zero"]
           (error-of "CREATE SEQUENCE s INCREMENT BY 0")))
    (is (= ["22023" "START value (1) cannot be less than MINVALUE (10)"]
           (error-of "CREATE SEQUENCE s MINVALUE 10 START 1")))
    (is (= ["22023" "START value (200) cannot be greater than MAXVALUE (100)"]
           (error-of "CREATE SEQUENCE s START 200 MAXVALUE 100")))
    (is (= ["22023" "CACHE (0) must be greater than zero"]
           (error-of "CREATE SEQUENCE s CACHE 0")))
    (is (= ["22023" "MAXVALUE (100000) is out of range for sequence data type smallint"]
           (error-of "CREATE SEQUENCE s AS smallint MAXVALUE 100000")))
    (is (= ["22023" "sequence type must be smallint, integer, or bigint"]
           (error-of "CREATE SEQUENCE s AS numeric"))))

  (testing "min == max is rejected, though the message says 'must be less than'"
    ;; PG compares with >=, so this is not an off-by-one on our side.
    (is (= ["22023" "MINVALUE (5) must be less than MAXVALUE (5)"]
           (error-of "CREATE SEQUENCE s MINVALUE 5 MAXVALUE 5"))))

  (testing "42601 for duplicate options — NO MINVALUE collides with MINVALUE"
    (is (= ["42601" "conflicting or redundant options"]
           (error-of "CREATE SEQUENCE s INCREMENT 2 INCREMENT 3")))
    (is (= ["42601" "conflicting or redundant options"]
           (error-of "CREATE SEQUENCE s MINVALUE 5 NO MINVALUE")))
    (is (= ["42601" "conflicting or redundant options"]
           (error-of "CREATE SEQUENCE s CYCLE NO CYCLE"))))

  (testing "options PG parses but refuses in this position"
    (is (= ["42601" "invalid sequence option SEQUENCE NAME"]
           (error-of "CREATE SEQUENCE s SEQUENCE NAME x")))
    ;; PG reaches this through a bare elog, so XX000 is faithful.
    (is (= ["XX000" "option \"logged\" not recognized"]
           (error-of "CREATE SEQUENCE s LOGGED")))))

(deftest test-validation-order-matches-postgres
  (testing "a statement with several problems reports the one PG reports"
    ;; INCREMENT is checked before MINVALUE/MAXVALUE and before CACHE,
    ;; so the zero-increment error wins over the min/max crosscheck.
    (is (= ["22023" "INCREMENT must not be zero"]
           (error-of "CREATE SEQUENCE s INCREMENT 0 MINVALUE 10 MAXVALUE 5 CACHE 0")))
    ;; With a valid increment, MINVALUE >= MAXVALUE wins over the CACHE check.
    (is (= ["22023" "MINVALUE (10) must be less than MAXVALUE (5)"]
           (error-of "CREATE SEQUENCE s MINVALUE 10 MAXVALUE 5 CACHE 0")))))

;; ============================================================================
;; Stored entity
;; ============================================================================

(deftest test-sequence-entity-offsets-the-counter
  (testing "the stored value is start - increment so nextval lands on start"
    (let [params (params-of "CREATE SEQUENCE s INCREMENT 20 START WITH 400")
          e (ddl/sequence-entity "s" params)]
      (is (= 380 (:__seq__/value e)))
      (is (= 20 (:__seq__/increment e)))
      (is (= 400 (:__seq__/start e)))
      (is (= 9223372036854775807 (:__seq__/maxvalue e)))
      (is (false? (:__seq__/cycle e))))))

(deftest test-translate-carries-if-not-exists
  (testing "the flag comes from the classifier, not a re-scan of the SQL"
    (let [r (ddl/translate-create-sequence
             (cls/classify "CREATE SEQUENCE IF NOT EXISTS s INCREMENT 20"))]
      (is (= :ddl-create-sequence (:type r)))
      (is (true? (:if-not-exists? r)))
      (is (= "s" (:seq-name r)))
      (is (= 20 (-> r :seq-params :increment))))))
