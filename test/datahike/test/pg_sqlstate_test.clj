(ns datahike.test.pg-sqlstate-test
  "Tests for PostgreSQL SQLSTATE code classification and propagation.

   PG clients (psycopg2, pgJDBC, asyncpg, Odoo) branch on SQLSTATE, e.g.
   Odoo retries 40001 (serialization_failure), ORMs map 23505 to unique
   violations, 23502 to not-null. Returning XX000 / 42000 for everything
   makes the client can't recover automatically and user error messages
   unhelpful.

   See postgres/src/backend/utils/errcodes.txt for the canonical code
   list."
  (:require [clojure.test :refer [deftest testing is]]
            [datahike.pg.errors :as errors]
            [datahike.pg.sql.fns :as fns]))

(deftest test-classify-ex-data-sqlstate
  (testing "explicit :sqlstate in ex-data wins over everything else"
    (let [e (ex-info "boom" {:sqlstate "55P03" :error :anything})]
      (is (= "55P03" (first (errors/classify-exception e)))))))

(deftest test-classify-ex-data-error-key
  (testing "Datahike :transact/schema → invalid_text_representation"
    (let [e (ex-info "bad value" {:error :transact/schema})]
      (is (= "22P02" (first (errors/classify-exception e))))))

  (testing "Datahike :transact/upsert → unique_violation"
    (let [e (ex-info "upsert conflict" {:error :transact/upsert})]
      (is (= "23505" (first (errors/classify-exception e))))))

  (testing "Datahike :db.unique/identity → unique_violation"
    (let [e (ex-info "duplicate identity" {:error :db.unique/identity})]
      (is (= "23505" (first (errors/classify-exception e))))))

  (testing "Datahike :query/invalid-clause → syntax_error"
    (let [e (ex-info "bad clause" {:error :query/invalid-clause})]
      (is (= "42601" (first (errors/classify-exception e)))))))

(deftest test-classify-message-pattern
  (testing "'Bad entity value ... does not match schema' → 22P02"
    (let [e (ex-info "Bad entity value \"foo\" at [:db/add 1 :a \"foo\"], value does not match schema definition. Must be conform to: (= (class %) java.lang.Long)"
                     {})]
      (is (= "22P02" (first (errors/classify-exception e))))))

  (testing "'cannot be cast to class' → 22P02 (invalid text representation)"
    (let [e (ex-info "class java.lang.Long cannot be cast to class clojure.lang.Keyword" {})]
      (is (= "22P02" (first (errors/classify-exception e))))))

  (testing "'unique constraint' → 23505"
    (let [e (ex-info "unique constraint violated on :person/email" {})]
      (is (= "23505" (first (errors/classify-exception e))))))

  (testing "'NOT NULL constraint' → 23502"
    (let [e (ex-info "NOT NULL constraint failed" {})]
      (is (= "23502" (first (errors/classify-exception e))))))

  (testing "'SQL parse error' → 42601 (syntax_error)"
    (let [e (ex-info "SQL parse error: unexpected token" {})]
      (is (= "42601" (first (errors/classify-exception e))))))

  (testing "'ParseException' → 42601"
    (let [e (ex-info "net.sf.jsqlparser.parser.ParseException: ..." {})]
      (is (= "42601" (first (errors/classify-exception e))))))

  (testing "'relation does not exist' → 42P01 (undefined_table)"
    (let [e (ex-info "relation \"foo\" does not exist" {})]
      (is (= "42P01" (first (errors/classify-exception e))))))

  (testing "'column does not exist' → 42703 (undefined_column)"
    (let [e (ex-info "column \"bar\" does not exist" {})]
      (is (= "42703" (first (errors/classify-exception e)))))))

(deftest test-classify-fallback
  (testing "unknown exception → XX000 (internal_error)"
    (let [e (RuntimeException. "something weird")]
      (is (= "XX000" (first (errors/classify-exception e)))))))

(deftest test-classify-precedence
  (testing "ex-data :error beats message regex"
    ;; Message says "unique" but ex-data says :transact/schema → 22P02 not 23505
    (let [e (ex-info "unique-sounding message but schema is the actual issue"
                     {:error :transact/schema})]
      (is (= "22P02" (first (errors/classify-exception e)))))))

(deftest test-cause-chain-walk
  (testing "ex-data hidden two levels deep (Datahike writer wrap) is still found"
    ;; Datahike's async writer re-wraps the real ExceptionInfo inside an
    ;; ExecutionException plus an outer ExceptionInfo with empty ex-data —
    ;; classify-exception must walk the cause chain to find :error.
    (let [inner (ex-info "inner" {:error :transact/unique :attribute :person/nick})
          mid   (java.util.concurrent.ExecutionException. inner)
          outer (ex-info "outer" {} mid)]
      (is (= "23505" (first (errors/classify-exception outer)))))))

(deftest test-datahike-error-taxonomy-coverage
  (testing "every Datahike :error key maps to a non-XX000 SQLSTATE"
    ;; Generated from grep of Datahike source (as of 0.6.1611). Update
    ;; when a new Datahike release adds an error key.
    (doseq [[k expected]
            [[:transact/schema           "22P02"]
             [:retract/schema            "22P02"]
             [:schema/validation         "22P02"]
             [:transact/upsert           "23505"]
             [:transact/unique           "23505"]
             [:transact/syntax           "42601"]
             [:entity-id/missing         "42704"]
             [:entity-id/syntax          "22P02"]
             [:lookup-ref/syntax         "22P02"]
             [:lookup-ref/unique         "23505"]
             [:query/binding             "42601"]
             [:query/where               "42601"]
             [:transact/cas              "40001"]
             [:transact/ensure           "23514"]
             [:transact/purge            "22023"]
             [:search/pattern            "22023"]
             [:merge/sync-not-supported  "0A000"]
             [:transact/sync-not-supported "0A000"]
             [:transaction/filtered      "0A000"]
             [:import/mismatch           "22P02"]]]
      (let [code (first (errors/classify-exception (ex-info "x" {:error k})))]
        (is (= expected code)
            (str k " should map to " expected ", got " code))))))

(deftest test-error-response-fields
  (testing "unique violation extracts constraint/table/column from :attribute"
    (let [e (ex-info "dup" {:error :transact/unique
                            :attribute :person/nick
                            :datom [nil :person/nick "alice"]})
          [_ _ fields] (errors/classify-exception e)]
      (is (= "person_nick_key" (.get fields "n")))
      (is (= "person" (.get fields "t")))
      (is (= "nick"   (.get fields "c")))
      (is (.contains ^String (.get fields "D") "alice"))))

  (testing "schema error extracts data type"
    (let [e (ex-info "bad value"
                     {:error :transact/schema
                      :attribute :t/age
                      :value "notanumber"
                      :schema {:db/valueType :db.type/long}})
          [_ _ fields] (errors/classify-exception e)]
      (is (= "t" (.get fields "t")))
      (is (= "age" (.get fields "c")))
      (is (= "long" (.get fields "d"))))))

;; ============================================================================
;; Pgwire-side error categories (the new error-categories registry)
;; ============================================================================

(deftest test-undefined-column-formats-pg-message
  (testing "{:error :undefined-column :table … :column …} produces a PG-shaped message + 42703"
    (let [e (ex-info "internal: column resolution failed"
                     {:error :undefined-column :table "employee" :column "dept_id"})
          [code msg fields] (errors/classify-exception e)]
      (is (= "42703" code))
      (is (= "column \"dept_id\" of relation \"employee\" does not exist" msg))
      (is (= "employee" (.get fields "t")))
      (is (= "dept_id" (.get fields "c"))))))

(deftest test-undefined-table
  (let [e (ex-info "x" {:error :undefined-table :table "nonsuch"})
        [code msg _] (errors/classify-exception e)]
    (is (= "42P01" code))
    (is (= "relation \"nonsuch\" does not exist" msg))))

(deftest test-undefined-database
  (let [e (ex-info "x" {:error :undefined-database :database "missing"})
        [code msg _] (errors/classify-exception e)]
    (is (= "3D000" code))
    (is (= "database \"missing\" does not exist" msg))))

(deftest test-not-null-violation
  (let [e (ex-info "x" {:error :not-null-violation :table "t" :column "c"})
        [code msg _] (errors/classify-exception e)]
    (is (= "23502" code))
    (is (re-find #"null value in column \"c\"" msg))))

(deftest test-unique-violation-with-constraint
  (let [e (ex-info "x" {:error :unique-violation
                        :table "person" :column "email"
                        :constraint "person_email_key"
                        :value "alice@example.com"})
        [code msg fields] (errors/classify-exception e)]
    (is (= "23505" code))
    (is (re-find #"duplicate key value violates unique constraint \"person_email_key\"" msg))
    (is (= "person_email_key" (.get fields "n")))))

(deftest test-feature-not-supported
  (let [e (ex-info "x" {:error :feature-not-supported :feature "GRANT"})
        [code msg _] (errors/classify-exception e)]
    (is (= "0A000" code))
    (is (= "GRANT is not supported" msg))))

(deftest test-division-by-zero-category
  (testing ":division-by-zero → 22012 with PG's exact message"
    (let [e (errors/pg-error :division-by-zero {})
          [code msg _] (errors/classify-exception e)]
      (is (= "22012" code))
      (is (= "division by zero" msg))
      ;; Paths that bypass classify-exception use .getMessage directly.
      (is (= "division by zero" (.getMessage e))))))

(deftest test-classify-message-divide-by-zero
  (testing "raw ArithmeticException 'Divide by zero' (quot, aggregates) → 22012"
    (let [e (ArithmeticException. "Divide by zero")]
      (is (= "22012" (first (errors/classify-exception e))))))

  (testing "'division by zero' message without ex-data → 22012"
    (let [e (ex-info "division by zero" {})]
      (is (= "22012" (first (errors/classify-exception e)))))))

(deftest test-sql-div-mod-raise-22012
  (testing "sql-div by zero raises 22012 for integer, float, and decimal divisors"
    (doseq [[a b] [[1 0] [1.0 0.0] [1M 0M] [1 0.0] [5 -0.0] [1 0M]]]
      (let [e (try (fns/sql-div a b) nil (catch Exception e e))]
        (is (some? e) (str a " / " b " should throw"))
        (when e
          (is (= "division by zero" (.getMessage ^Exception e)))
          (is (= "22012" (first (errors/classify-exception e))))))))

  (testing "sql-mod with zero modulus raises 22012"
    (doseq [[a b] [[1 0] [1.0 0.0] [1M 0M]]]
      (let [e (try (fns/sql-mod a b) nil (catch Exception e e))]
        (is (some? e) (str a " % " b " should throw"))
        (when e
          (is (= "division by zero" (.getMessage ^Exception e)))
          (is (= "22012" (first (errors/classify-exception e))))))))

  (testing "NULL propagates before the zero check — NULL / 0 is NULL, not an error"
    (is (= :__null__ (fns/sql-div :__null__ 0)))
    (is (= :__null__ (fns/sql-div nil 0)))
    (is (= :__null__ (fns/sql-div 1 :__null__)))
    (is (= :__null__ (fns/sql-mod :__null__ 0)))
    (is (= :__null__ (fns/sql-mod 1 :__null__))))

  (testing "non-zero divisors still divide"
    (is (= 3 (fns/sql-div 6 2)))
    (is (= 1 (fns/sql-mod 7 2)))))

(deftest test-query-canceled
  (let [e (ex-info "x" {:error :query-canceled})
        [code msg _] (errors/classify-exception e)]
    (is (= "57014" code))
    (is (re-find #"user request" msg)))
  (let [e (errors/pg-error :query-canceled
                           {:message "secondary-index build was canceled"})
        [code msg _] (errors/classify-exception e)]
    (is (= "57014" code))
    (is (= "secondary-index build was canceled" msg))
    (is (= msg (ex-message e)))))

(deftest test-explicit-sqlstate-skips-formatter
  (testing "explicit :sqlstate wins over :error category — message stays as-is"
    (let [e (ex-info "boom"
                     {:sqlstate "57P03"
                      :error :undefined-column :table "x" :column "y"})
          [code msg _] (errors/classify-exception e)]
      (is (= "57P03" code))
      (is (= "boom" msg)))))

;; ============================================================================
;; Datahike-message rewriting — the SQLAlchemy regression
;; ============================================================================

(deftest test-rewrites-undefined-attribute-to-undefined-column
  (testing "Datahike's `Bad entity attribute …` for an unknown attr → 42703 with PG vocabulary"
    (let [e (ex-info "Bad entity attribute :employee/dept_id at {:db/id 47, :employee/dept_id 1}, not defined in current schema"
                     {:error :transact/schema})
          [code msg fields] (errors/classify-exception e)]
      (is (= "42703" code))
      (is (= "column \"dept_id\" of relation \"employee\" does not exist" msg))
      (is (= "employee" (.get fields "t")))
      (is (= "dept_id"  (.get fields "c"))))))

(deftest test-rewrites-bad-entity-value-to-22P02-with-clean-message
  (testing "Datahike's `Bad entity value … at [:db/add ID :ns/col VALUE]` rewrites the message to PG vocabulary"
    (let [e (ex-info "Bad entity value 47 at [:db/add 99 :person/age oops], value '47' does not match schema definition"
                     {:error :transact/schema})
          [code msg _] (errors/classify-exception e)]
      (is (= "22P02" code))
      (is (re-find #"invalid input syntax for column \"age\" of relation \"person\"" msg)))))

;; ============================================================================
;; Cause-chain message walk
;; ============================================================================

(deftest test-walks-cause-chain-for-message
  (testing "outer wrapper without a useful message — pull from cause"
    (let [inner (ex-info "Bad entity attribute :employee/dept_id at X, not defined in current schema"
                         {:error :transact/schema})
          outer (RuntimeException. "" inner)
          [code msg _] (errors/classify-exception outer)]
      (is (= "42703" code))
      (is (re-find #"column \"dept_id\"" msg)))))

;; ============================================================================
;; Math function domain errors (issue #22)
;; ============================================================================
;;
;; Every expectation below was captured from PostgreSQL 17.10 — both the
;; SQLSTATE and the exact errmsg string. Java's Math.* returns NaN or
;; Infinity for all of these; PG raises. See the "Math function
;; implementations" section in datahike.pg.sql.fns for why each check
;; exists.

(defn- call-sql-fn
  "Invoke a mapped SQL function by name, as the translator does."
  [fname & args]
  (apply (get fns/sql-fn->clj-fn fname) args))

(defn- sql-fn-error
  "Return [sqlstate message] for a call expected to raise, or nil."
  [fname & args]
  (try (apply call-sql-fn fname args) nil
       (catch Exception e
         (let [[code msg _] (errors/classify-exception e)] [code msg]))))

(deftest test-math-domain-errors
  (testing "sqrt of a negative number → 2201F (was NaN — issue #22)"
    (is (= ["2201F" "cannot take square root of a negative number"]
           (sql-fn-error "sqrt" -5.0)))
    (is (= ["2201F" "cannot take square root of a negative number"]
           (sql-fn-error "sqrt" -42))))

  (testing "logarithm domain → 2201E"
    (is (= ["2201E" "cannot take logarithm of zero"] (sql-fn-error "ln" 0.0)))
    (is (= ["2201E" "cannot take logarithm of a negative number"]
           (sql-fn-error "ln" -1.0)))
    (is (= ["2201E" "cannot take logarithm of zero"] (sql-fn-error "log" 0.0)))
    (is (= ["2201E" "cannot take logarithm of a negative number"]
           (sql-fn-error "log10" -1.0)))
    (is (= ["2201E" "cannot take logarithm of zero"] (sql-fn-error "log" 0.0 10.0))))

  (testing "log(1, x) is division by zero in PG, not a logarithm error"
    (is (= ["22012" "division by zero"] (sql-fn-error "log" 1.0 10.0))))

  (testing "power domain → 2201F"
    (is (= ["2201F" "zero raised to a negative power is undefined"]
           (sql-fn-error "power" 0.0 -1.0)))
    (is (= ["2201F" "a negative number raised to a non-integer power yields a complex result"]
           (sql-fn-error "power" -2.0 0.5))))

  (testing "asin/acos outside [-1,1] → 22003"
    (is (= ["22003" "input is out of range"] (sql-fn-error "asin" 2.0)))
    (is (= ["22003" "input is out of range"] (sql-fn-error "acos" -2.0))))

  (testing "overflow to infinity from finite inputs → 22003"
    (is (= ["22003" "value out of range: overflow"] (sql-fn-error "exp" 10000.0)))
    (is (= ["22003" "value out of range: overflow"] (sql-fn-error "power" 2.0 10000.0))))

  (testing "UNDERFLOW is an error too — exp(-1000::float8) is not 0 in PG"
    (is (= ["22003" "value out of range: underflow"] (sql-fn-error "exp" -1000.0))))

  (testing "trig rejects an infinite argument (Java returns NaN)"
    (doseq [fname ["sin" "cos" "tan" "cot"]]
      (is (= ["22003" "input is out of range"]
             (sql-fn-error fname Double/POSITIVE_INFINITY))
          (str fname "(Infinity)"))))

  (testing "acosh/atanh domain → 22003"
    (is (= ["22003" "input is out of range"] (sql-fn-error "acosh" 0.5)))
    (is (= ["22003" "input is out of range"] (sql-fn-error "atanh" 2.0))))

  (testing "width_bucket argument failures are 2201G, not 22003"
    (is (= ["2201G" "count must be greater than zero"]
           (sql-fn-error "width_bucket" 5.0 0.0 10.0 0)))
    (is (= ["2201G" "lower bound cannot equal upper bound"]
           (sql-fn-error "width_bucket" 5.0 3.0 3.0 5)))
    (is (= ["2201G" "lower and upper bounds cannot be NaN"]
           (sql-fn-error "width_bucket" 5.0 Double/NaN 10.0 5)))
    (is (= ["2201G" "lower and upper bounds must be finite"]
           (sql-fn-error "width_bucket" 5.0 0.0 Double/POSITIVE_INFINITY 5)))))

(deftest test-math-non-errors-that-look-like-errors
  (testing "sinh/cosh overflow to Infinity WITHOUT error, unlike exp"
    (is (Double/isInfinite (double (call-sql-fn "sinh" 1000.0))))
    (is (Double/isInfinite (double (call-sql-fn "cosh" 1000.0)))))

  (testing "cot(0) is Infinity, not an error"
    (is (Double/isInfinite (double (call-sql-fn "cot" 0.0)))))

  (testing "atanh(±1) is ±Infinity, not an error"
    (is (Double/isInfinite (double (call-sql-fn "atanh" 1.0))))
    (is (Double/isInfinite (double (call-sql-fn "atanh" -1.0)))))

  (testing "acosh(NaN) is NaN — NaN slips past the `< 1.0` guard, as in PG"
    (is (Double/isNaN (double (call-sql-fn "acosh" Double/NaN)))))

  (testing "float8 sign(NaN) is 0 in PG, not NaN as Math/signum gives"
    (is (= 0.0 (call-sql-fn "sign" Double/NaN))))

  (testing "width_bucket with low > high mirror-reverses rather than erroring"
    (is (= 3 (call-sql-fn "width_bucket" 5.0 10.0 0.0 5)))))

(deftest test-math-nan-propagates-without-error
  (testing "NaN input propagates — PG does NOT raise a domain error for it"
    (doseq [fname ["sqrt" "ln" "log10" "asin" "acos" "exp"]]
      (let [r (call-sql-fn fname Double/NaN)]
        (is (and (number? r) (Double/isNaN (double r)))
            (str fname "(NaN) should be NaN, got " r)))))

  (testing "NaN in either power argument yields NaN"
    (is (Double/isNaN (double (call-sql-fn "power" Double/NaN 2.0))))
    (is (Double/isNaN (double (call-sql-fn "power" 2.0 Double/NaN))))))

(deftest test-math-values-match-postgres
  (testing "log is base 10, not natural log — ln is natural log"
    ;; The old mapping sent `log` to Math/log, silently returning
    ;; 4.605… for log(100): a wrong ANSWER, not an error.
    (is (= 2.0 (call-sql-fn "log" 100.0)))
    (is (= 2.0 (call-sql-fn "log10" 100.0)))
    ;; PostgreSQL has only a NUMERIC two-argument log -- there is no
    ;; float8 overload -- so both arguments coerce and the result is
    ;; numeric, carrying numeric_log's scale.
    (is (= (bigdec "3.0000000000000000") (call-sql-fn "log" 2.0 8.0)))
    (is (< (Math/abs (- 0.6931471805599453 (double (call-sql-fn "ln" 2.0)))) 1e-15)))

  (testing "round breaks ties away from zero, like PG (Math/round rounds toward +inf)"
    (is (= 3 (call-sql-fn "round" 2.5)))
    (is (= -3 (call-sql-fn "round" -2.5)))
    (is (= 0 (compare 2.46M (call-sql-fn "round" 2.4567 2)))))

  (testing "trunc rounds toward zero"
    (is (= 1 (call-sql-fn "trunc" 1.7)))
    (is (= -1 (call-sql-fn "trunc" -1.7))))

  (testing "cbrt of a negative number is defined (unlike sqrt)"
    (is (= -2.0 (call-sql-fn "cbrt" -8.0))))

  (testing "gcd / lcm / width_bucket"
    (is (= 0 (compare (biginteger 4) (call-sql-fn "gcd" 12 8))))
    (is (= 0 (compare (biginteger 24) (call-sql-fn "lcm" 12 8))))
    (is (= 0 (compare (biginteger 0) (call-sql-fn "lcm" 0 8))))
    (is (= 3 (call-sql-fn "width_bucket" 5.0 0.0 10.0 5)))
    (is (= 0 (call-sql-fn "width_bucket" -1.0 0.0 10.0 5)))
    (is (= 6 (call-sql-fn "width_bucket" 99.0 0.0 10.0 5)))))

(deftest test-math-arity-is-42883
  (testing "wrong argument count resolves as 42883, not a runtime XX000"
    (let [[code msg _] (try (fns/check-arity! "sqrt" 2) nil
                            (catch Exception e (errors/classify-exception e)))]
      (is (= "42883" code))
      ;; PG's wording for untyped literals: sqrt('a','b') reports
      ;; "function sqrt(unknown, unknown) does not exist".
      (is (= "function sqrt(unknown, unknown) does not exist" msg)))
    (let [[code _] (try (fns/check-arity! "pi" 1) nil
                        (catch Exception e (errors/classify-exception e)))]
      (is (= "42883" code))))

  (testing "accepted arities pass"
    (is (nil? (fns/check-arity! "log" 1)))
    (is (nil? (fns/check-arity! "log" 2)))
    (is (nil? (fns/check-arity! "round" 2)))
    (is (nil? (fns/check-arity! "pi" 0))))

  (testing "unregistered names are unchecked"
    (is (nil? (fns/check-arity! "pg_get_expr" 7)))))
