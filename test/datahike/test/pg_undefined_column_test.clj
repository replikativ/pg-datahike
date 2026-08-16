(ns datahike.test.pg-undefined-column-test
  "An unknown column must raise 42703, not read as NULL.

   This layer stores rows as EAV, and a missing attribute is naturally
   NULL — so a column that does not exist was translated into the same
   `get-else … :__null__` binding as one that does. Internally
   consistent, but the consequences were:

     SELECT nosuchcol FROM t          -> a row of NULLs
     SELECT id FROM t WHERE nosuchcol = 1  -> zero rows

   Both are answers, not errors, so a typo'd column name in an
   application looks like data. PostgreSQL rejects the reference at
   parse-analyze.

   The permissiveness that IS wanted survives untouched: a row that
   simply has no value for a column the table DOES have still reads as
   NULL. The test is whether the attribute is in the schema, which is
   exact here because this layer is schema-on-write.

   Two funnels needed the check, which is why both are covered below:
   `col-var!` (projections, ORDER BY, expressions) and `plain-join-col`
   (the index-seekable data patterns that `col = $N` / `col = <literal>`
   compile to, which bypass col-var! entirely)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*handler* (pg/make-query-handler conn)]
          (.execute *handler*
                    "CREATE TABLE t (id int PRIMARY KEY, name text, maybe text)")
          (.execute *handler* "INSERT INTO t (id, name) VALUES (1, 'a')")
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- err [sql]
  (try (.-error ^PgWireServer$QueryResult (.execute *handler* sql))
       (catch Exception e (ex-message e))))

(defn- rows [sql]
  (mapv vec (.-rows ^PgWireServer$QueryResult (.execute *handler* sql))))

(defn- undefined-column? [sql]
  (= "column \"nosuchcol\" does not exist" (err sql)))

;; ---------------------------------------------------------------------------
;; The projection funnel (col-var!)
;; ---------------------------------------------------------------------------

(deftest unknown-column-in-projection
  (testing "returned a row of NULLs"
    (is (undefined-column? "SELECT nosuchcol FROM t"))
    (is (undefined-column? "SELECT id, nosuchcol FROM t"))
    (is (undefined-column? "SELECT t.nosuchcol FROM t"))
    (is (undefined-column? "SELECT nosuchcol FROM t alias"))))

(deftest unknown-column-in-order-by
  (is (undefined-column? "SELECT id FROM t ORDER BY nosuchcol")))

(deftest unknown-column-in-an-expression
  (is (undefined-column? "SELECT nosuchcol + 1 FROM t"))
  (is (undefined-column? "SELECT upper(nosuchcol) FROM t")))

;; ---------------------------------------------------------------------------
;; The data-pattern funnel (plain-join-col) — `col = value` compiles to
;; an index-seekable pattern that never reaches col-var!
;; ---------------------------------------------------------------------------

(deftest unknown-column-in-where
  (testing "returned zero rows — the shape that makes a typo invisible"
    (is (undefined-column? "SELECT id FROM t WHERE nosuchcol = 1"))
    (is (undefined-column? "SELECT id FROM t WHERE nosuchcol = 'x'"))
    (is (undefined-column? "SELECT id FROM t WHERE nosuchcol IS NULL"))
    (is (undefined-column? "SELECT id FROM t WHERE nosuchcol > 5"))))

(deftest unknown-column-in-a-join-condition
  (.execute *handler* "CREATE TABLE u (id int PRIMARY KEY, t_id int)")
  (is (undefined-column? "SELECT t.id FROM t JOIN u ON u.nosuchcol = t.id")))

;; ---------------------------------------------------------------------------
;; What must still be permissive
;; ---------------------------------------------------------------------------

(deftest a-column-the-table-has-still-reads-as-null
  (testing "the EAV permissiveness that is actually wanted"
    (is (= [["1" "a" nil]] (rows "SELECT id, name, maybe FROM t")))
    (is (nil? (err "SELECT maybe FROM t")))
    (is (= [["1"]] (rows "SELECT id FROM t WHERE maybe IS NULL")))))

(deftest known-columns-are-unaffected
  (is (= [["1" "a"]] (rows "SELECT id, name FROM t")))
  (is (= [["1"]] (rows "SELECT id FROM t WHERE name = 'a'")))
  (is (= [["1"]] (rows "SELECT id FROM t WHERE id = 1")))
  (is (= [["a"]] (rows "SELECT x.name FROM (SELECT name FROM t) x"))))

(deftest an-unknown-table-is-still-a-relation-error
  (testing "42P01, not a column error — the distinction PG draws"
    (is (re-find #"relation \"nope\" does not exist"
                 (or (err "SELECT id FROM nope") "")))))

(deftest catalog-columns-stay-permissive
  (testing "client introspection legitimately asks for pg_catalog columns
            we don't materialise; those must degrade to NULL rather than
            fail, which is what the empty-catalog machinery is for"
    (is (nil? (err "SELECT relname FROM pg_class WHERE relname = 't'")))
    (is (= [["t"]] (rows "SELECT relname FROM pg_class WHERE relname = 't'")))))
