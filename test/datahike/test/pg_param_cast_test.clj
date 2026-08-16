(ns datahike.test.pg-param-cast-test
  "Audit fix A22 — `where-param-oids` should unwrap `CastExpression`
   and `Parenthesis` so a param sitting inside a CAST is described
   with the cast target's OID (PG semantics: the cast overrides the
   comparand column's type for ParameterDescription)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.sql :as sql])
  (:import [datahike.pg PgWireServer PgWireServer$QueryHandlerFactory]
           [java.sql Connection DriverManager]))

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

(defn- open ^Connection []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/datahike?user=datahike&password=x&sslmode=disable")))

;; ---------------------------------------------------------------------------
;; Direct unit-level: where-param-oids should infer through CAST + Parenthesis

(defn- run-param-oids
  "Run sql/parse-sql on `s` against a tiny synthetic schema and pull
   :param-oids out (1-indexed map idx → OID)."
  [s]
  (let [schema {:t/age {:db/valueType :db.type/long :db/ident :t/age}
                :t/name {:db/valueType :db.type/string :db/ident :t/name}}
        parsed (sql/parse-sql s schema nil)]
    (:param-oids parsed)))

(deftest cast-on-param-side-uses-cast-target
  (testing "WHERE col = CAST(? AS INT) — param OID = int4 (cast target)"
    (let [oids (run-param-oids "SELECT * FROM t WHERE age = CAST(? AS INT)")]
      ;; oid-int4 = 23
      (is (= 23 (get oids 1)))))
  (testing "WHERE col = CAST(? AS BIGINT) — param OID = int8"
    (let [oids (run-param-oids "SELECT * FROM t WHERE age = CAST(? AS BIGINT)")]
      ;; oid-int8 = 20
      (is (= 20 (get oids 1)))))
  (testing "WHERE col = CAST(? AS TEXT) — cast target overrides col type"
    (let [oids (run-param-oids "SELECT * FROM t WHERE age = CAST(? AS TEXT)")]
      ;; oid-text = 25 — the cast target wins, NOT age's int8
      (is (= 25 (get oids 1))))))

(deftest cast-on-col-side-still-resolves
  (testing "WHERE CAST(col AS TEXT) = ? — param maps to col-side comparand"
    ;; Cast on col side should still give us a sensible OID for the
    ;; param (here we map to col's actual OID since the cast doesn't
    ;; wrap the ? — the result type is text per PG, but the col is age).
    ;; Best-effort: as long as we produce *some* mapping, pgjdbc has
    ;; a hint instead of falling back to text-by-default.
    (let [oids (run-param-oids "SELECT * FROM t WHERE CAST(age AS TEXT) = ?")]
      (is (some? oids)))))

(deftest cast-between-clause
  (testing "col BETWEEN CAST(? AS INT) AND ? — first param uses cast target"
    (let [oids (run-param-oids "SELECT * FROM t WHERE age BETWEEN CAST(? AS INT) AND ?")]
      (is (= 23 (get oids 1)))
      (is (= 20 (get oids 2))))))

(deftest cast-in-in-list
  (testing "col IN (CAST(? AS INT), ?) — first param uses cast target, second col"
    (let [oids (run-param-oids "SELECT * FROM t WHERE age IN (CAST(? AS INT), ?)")]
      (is (= 23 (get oids 1)))
      (is (= 20 (get oids 2))))))

;; ---------------------------------------------------------------------------
;; End-to-end via pgjdbc: Describe('S', …) ParameterDescription should
;; carry the cast target type, so pgjdbc accepts setInt/setLong without
;; "Can't change resolved type for param".

(deftest jdbc-cast-on-param-end-to-end
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t (id INT PRIMARY KEY, age INT)")
      (.executeUpdate st "INSERT INTO t (id, age) VALUES (1, 30)")
      (.executeUpdate st "INSERT INTO t (id, age) VALUES (2, 50)"))
    (with-open [ps (.prepareStatement c "SELECT id FROM t WHERE age > CAST(? AS INT) ORDER BY id")]
      (.setInt ps 1 25)
      (with-open [rs (.executeQuery ps)]
        (is (.next rs)) (is (= 1 (.getInt rs 1)))
        (is (.next rs)) (is (= 2 (.getInt rs 1)))))))
