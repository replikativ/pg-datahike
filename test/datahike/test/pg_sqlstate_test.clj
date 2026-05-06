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
            [datahike.pg.errors :as errors]))

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

(deftest test-query-canceled
  (let [e (ex-info "x" {:error :query-canceled})
        [code msg _] (errors/classify-exception e)]
    (is (= "57014" code))
    (is (re-find #"user request" msg))))

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
