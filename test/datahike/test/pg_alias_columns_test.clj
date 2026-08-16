(ns datahike.test.pg-alias-columns-test
  "Column-alias lists on a FROM item — `generate_series(1,3) AS s(r)`.

   PostgreSQL renames a relation's columns positionally through the
   alias. We ignored the list entirely and kept the ALIAS name as the
   column name, so `SELECT r FROM generate_series(1,3) AS s(r)` bound
   nothing: three rows of NULL before the unknown-column check landed,
   and a hard 42703 after it.

   That is the shape pgjdbc's TypeInfoCache probe uses, which is why
   `ResultSet.getObject` on a non-trivial column type failed.

   The same probe also passes a NON-CONSTANT argument to the set-
   returning function — `generate_series(1, array_upper(current_schemas(
   false), 1))`. The materialiser only understood literals, so it
   declined and the alias resolved to nothing. Its `eval-fn` seam now
   falls back to evaluating a constant scalar expression; a genuinely
   correlated argument still fails to evaluate standalone and is
   reported as correlated exactly as before.

   Expectations captured from PostgreSQL 17."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (binding [*handler* (pg/make-query-handler conn)] (f))
           (finally (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- rows [sql] (mapv vec (.-rows ^PgWireServer$QueryResult (.execute *handler* sql))))
(defn- state [sql]
  (try (.-sqlstate ^PgWireServer$QueryResult (.execute *handler* sql))
       (catch Exception e (:sqlstate (ex-data e)))))
(defn- err [sql]
  (try (.-error ^PgWireServer$QueryResult (.execute *handler* sql))
       (catch Exception e (ex-message e))))

(deftest alias-column-list-renames-positionally
  (testing "the column takes the ALIAS-LIST name, not the alias"
    (is (= [["1"] ["2"] ["3"]] (rows "SELECT r FROM generate_series(1,3) AS s(r)")))
    (is (= [["1"] ["2"] ["3"]] (rows "SELECT s.r FROM generate_series(1,3) AS s(r)")))
    (is (= [["1"] ["2"]] (rows "SELECT n FROM unnest(ARRAY[1,2]) AS u(n)"))))
  (testing "no alias list — the alias still names a single-column SRF"
    (is (= [["1"] ["2"] ["3"]] (rows "SELECT s FROM generate_series(1,3) AS s")))
    (is (= [["1"] ["2"] ["3"]] (rows "SELECT * FROM generate_series(1,3) AS s"))))
  (testing "more names than columns is 42P10, as in PostgreSQL"
    (is (= "42P10" (state "SELECT r FROM generate_series(1,3) AS s(r, extra)")))
    (is (re-find #"has 1 columns available but 2 columns specified"
                 (or (err "SELECT r FROM generate_series(1,3) AS s(r, extra)") "")))))

(deftest constant-expression-srf-arguments
  (testing "a set-returning function whose argument is a constant
            EXPRESSION rather than a literal — the pgjdbc TypeInfoCache
            shape. The materialiser used to decline these outright."
    (is (= [["1"]] (rows "SELECT s.r FROM generate_series(1, array_upper(current_schemas(false), 1)) AS s(r)")))
    (is (= [["1"] ["2"] ["3"]] (rows "SELECT r FROM generate_series(1, 1 + 2) AS s(r)"))))
  (testing "plain literal arguments keep working"
    (is (= [["2"] ["3"]] (rows "SELECT r FROM generate_series(2,3) AS s(r)")))))
