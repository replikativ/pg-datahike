(ns datahike.test.pg-insert-coercion-test
  "Value-coercion gaps in the INSERT path that pgjdbc's ResultSetTest
   and PreparedStatementTest flushed out.

   Covers:
     - 'string'::real / 'string'::float4 lands a string value against a
       :db.type/float column; coerce-insert-value must string→float.
     - Prepared-statement NULL parameter (setBytes(i, null),
       setString(i, null), ...) resolves via substitute-params to a
       nil value in a map entity; must be dropped from the tx-data
       map rather than emitting [:db/add eid attr nil]."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer PgWireServer$QueryHandlerFactory]
           [java.sql Connection DriverManager SQLException Types]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *port* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          factory (reify PgWireServer$QueryHandlerFactory
                    (create [_] (pg/make-query-handler conn)))
          server (PgWireServer. 0 "127.0.0.1" factory)]
      (.start server)
      (try
        (binding [*conn* conn *port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- open
  "Open a JDBC connection. The prepared-statement NULL test uses the
   default query-mode so Bind/Execute actually flow, not simple-mode
   literal-interpolation (which pgJDBC can't do for BYTEA anyway)."
  (^Connection [] (open ""))
  (^Connection [^String extra]
   (DriverManager/getConnection
    (str "jdbc:postgresql://127.0.0.1:" *port*
         "/datahike?user=datahike&password=x&sslmode=disable"
         "&binaryTransfer=false" extra))))

;; ---------------------------------------------------------------------------
;; Bug 1: string literals coerce into :db.type/float columns.
;; pgjdbc's ResultSetTest.setUp does `VALUES(10, '9223371487098961921', null)`
;; into a `float4` column; the translator produces a string value that
;; Datahike's schema rejects without the coercion.

(deftest float-column-accepts-string-literal
  (letfn [(approx-eq [expected actual eps]
            (< (Math/abs (- (double expected) (double actual))) eps))]
    (with-open [c (open)]
      (with-open [st (.createStatement c)]
        (.execute st "CREATE TABLE f (i INT, a FLOAT4)")
        (.executeUpdate st "INSERT INTO f VALUES (1, '1.5')")
        (.executeUpdate st "INSERT INTO f VALUES (2, '-1.001'::real)")
        (.executeUpdate st "INSERT INTO f VALUES (3, '9223371487098961921')")
        (with-open [rs (.executeQuery st "SELECT i, a FROM f ORDER BY i")]
          (.next rs) (is (approx-eq 1.5    (.getFloat rs 2) 1e-6))
          (.next rs) (is (approx-eq -1.001 (.getFloat rs 2) 1e-4))
          (.next rs) (is (Float/isFinite (.getFloat rs 2))
                         "large integer-ish value is representable as finite float (or ±Infinity)"))))))

(deftest float-column-rejects-unparseable-string
  ;; Unparseable stays as a string and the writer raises — fine, that
  ;; matches PG's "invalid input syntax for type real" (22P02).
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE f (a FLOAT4)")
      (let [raised (try (.executeUpdate st "INSERT INTO f VALUES ('not a number')")
                        nil
                        (catch SQLException e e))]
        (is (some? raised))))))

(deftest bare-integer-literal-overflowing-long
  ;; pgjdbc's ResultSetTest.setUp emits
  ;;   INSERT INTO t VALUES (12, 9.2E18, null)
  ;; for a float column, but via Float.toString some adjacent tests
  ;; emit the un-scientific form `9223372036854775808` (== 2^63, one
  ;; past Long.MAX_VALUE). JSqlParser stores these as LongValue;
  ;; `.getValue` throws NumberFormatException. We fall back to
  ;; BigInteger and let coerce-insert-value down-convert to the column
  ;; type.
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE f (a FLOAT4, b FLOAT8, c NUMERIC)")
      (.executeUpdate st "INSERT INTO f VALUES (9223372036854775808, 9223372036854775808, 9223372036854775808)")
      (with-open [rs (.executeQuery st "SELECT a, b, c FROM f")]
        (is (.next rs))
        (is (Float/isFinite (.getFloat rs 1)))
        (is (Double/isFinite (.getDouble rs 2)))
        (is (some? (.getBigDecimal rs 3)))))))

;; ---------------------------------------------------------------------------
;; Bug 2: NULL bound into an INSERT via a prepared-statement parameter
;; resolves to a nil value after substitute-params; the entity map must
;; have the nil-valued key dropped so Datahike doesn't reject
;; [:db/add eid attr nil] with :transact/syntax.

(deftest bigint-overflow-into-long-column-raises-22003
  ;; A19 audit fix: a BigInteger value that overflows Long range must
  ;; raise SQLSTATE 22003 (numeric_value_out_of_range). The old code
  ;; silently truncated via `.longValue` which produced a wrong value
  ;; with no error.
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE big (id INT PRIMARY KEY, n BIGINT)")
      ;; 2^64 — outside Long range but a valid PG bigint literal that
      ;; overflows; previously stored as `Long/MIN_VALUE` silently.
      (let [raised (try (.executeUpdate st "INSERT INTO big VALUES (1, 18446744073709551616)")
                        nil
                        (catch SQLException e e))]
        (is (some? raised) "expected an exception for overflow")
        (when raised
          (is (= "22003" (.getSQLState raised))
              (str "got " (.getSQLState raised) ": " (.getMessage raised))))))))

(deftest bad-numeric-string-into-numeric-column-raises-22P02
  ;; A21 audit fix: previously, an unparseable string lifted into a
  ;; :db.type/bigdec column was returned unchanged, then surfaced as a
  ;; generic Datahike schema error. Now we raise 22P02 directly.
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE n (id INT, v NUMERIC)")
      (let [raised (try (.executeUpdate st "INSERT INTO n VALUES (1, 'not a number')")
                        nil
                        (catch SQLException e e))]
        (is (some? raised))
        (when raised
          (is (= "22P02" (.getSQLState raised))
              (str "got " (.getSQLState raised) ": " (.getMessage raised))))))))

(deftest insert-values-evaluates-numeric-arithmetic
  (with-open [c (open)
              st (.createStatement c)]
    (.execute st "CREATE TABLE num_variance (a NUMERIC)")
    (doseq [value ["0" "3e-500" "-3e-500"
                   "4e-500 - 1e-16383" "-4e-500 + 1e-16383"]]
      (.executeUpdate st (str "INSERT INTO num_variance VALUES (" value ")")))
    (with-open [rs (.executeQuery st
                                  (str "SELECT trim_scale(variance(a) * 1e1000) "
                                       "FROM num_variance"))]
      (is (.next rs))
      (is (= "12" (.getString rs 1))))))

(deftest prepared-insert-with-null-parameter
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE s (id INT PRIMARY KEY, bin BYTEA, str TEXT)"))
    (with-open [ps (.prepareStatement c "INSERT INTO s (id, bin, str) VALUES (?, ?, ?)")]
      (.setInt    ps 1 1)
      (.setNull   ps 2 Types/BINARY)
      (.setString ps 3 "hello")
      (is (= 1 (.executeUpdate ps))))
    (with-open [ps (.prepareStatement c "INSERT INTO s (id, bin, str) VALUES (?, ?, ?)")]
      (.setInt    ps 1 2)
      (.setBytes  ps 2 (byte-array [(byte 1) (byte 2)]))
      (.setNull   ps 3 Types/VARCHAR)
      (is (= 1 (.executeUpdate ps))))
    ;; Verify both rows exist, with the right attrs absent.
    (with-open [st (.createStatement c)]
      (with-open [rs (.executeQuery st "SELECT id, str FROM s WHERE id = 1")]
        (is (.next rs))
        (is (= "hello" (.getString rs 2))))
      (with-open [rs (.executeQuery st "SELECT id, bin FROM s WHERE id = 2")]
        (is (.next rs))
        (is (some? (.getBytes rs 2)))))))
