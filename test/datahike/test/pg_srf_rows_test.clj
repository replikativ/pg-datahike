(ns datahike.test.pg-srf-rows-test
  "Set-returning functions in FROM have to produce SCANNABLE rows.

   A table function is materialised into a virtual table in a
   speculative db. Two things were missing, and both failed silently —
   the queries were well-formed and answered, just with nothing in them.

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

(defn- run [sql] (.execute *handler* sql))
(defn- rows [sql] (mapv vec (.-rows ^PgWireServer$QueryResult (run sql))))

(deftest virtual-tables-carry-a-row-marker
  (testing "without a row-existence marker `count(*)` and `SELECT *` had
            no column to enumerate, so a scan returned ZERO rows even
            though the rows were there — `count(g)` on the same query
            correctly answered 10. sequence->virtual-table already
            transacts one and its comment names this exact hazard."
    (is (= [["10"]] (rows "SELECT count(*) FROM generate_series(1,10) g")))
    (is (= [["10"]] (rows "SELECT count(g) FROM generate_series(1,10) g")))
    (is (= [["3"]] (rows "SELECT count(*) FROM unnest(ARRAY[1,2,3]) u")))
    (is (= [["1"] ["2"] ["3"]] (rows "SELECT * FROM generate_series(1,3) g")))))

(deftest insert-select-from-a-set-returning-function
  (testing "the source query ran against the PLAIN db, where the virtual
            table does not exist, so the standard bulk-load idiom
            silently reported INSERT 0 0 while the same SELECT on its own
            returned its rows"
    (run "CREATE TABLE bulk (id int PRIMARY KEY, v int)")
    (is (= "INSERT 0 5"
           (.-commandTag ^PgWireServer$QueryResult
            (run "INSERT INTO bulk SELECT g, g*2 FROM generate_series(1,5) g"))))
    (is (= [["5"]] (rows "SELECT count(*) FROM bulk")))
    (is (= [["10"]] (rows "SELECT v FROM bulk WHERE id = 5"))))
  (testing "and the SELECT alone still works, which is what made the
            no-op hard to notice"
    (is (= 5 (count (rows "SELECT g, g*2 FROM generate_series(1,5) g"))))))

;; ---------------------------------------------------------------------------
;; Schema-qualified SRFs, and pg_get_keywords

(deftest schema-qualified-srf-in-from
  ;; `materialize-table-function` matched the RAW function name, so
  ;; `pg_catalog.generate_series(1,3)` missed its cond, returned nil, and
  ;; surfaced as the internal `Query for unknown vars: [?_eid]`.
  ;; PostgreSQL resolves the qualified and unqualified forms to the same
  ;; function through search_path, and pgjdbc writes the qualified one.
  (testing "generate_series resolves with or without the schema"
    (is (= [["3"]] (rows "SELECT count(*) FROM generate_series(1,3)")))
    (is (= [["3"]] (rows "SELECT count(*) FROM pg_catalog.generate_series(1,3)"))))

  (testing "the rows are the same either way"
    (is (= (rows "SELECT * FROM generate_series(1,3) AS g(n) ORDER BY n")
           (rows "SELECT * FROM pg_catalog.generate_series(1,3) AS g(n) ORDER BY n")))))

(deftest pg-get-keywords
  ;; pgjdbc calls this on every connection via getSQLKeywords():
  ;;   select string_agg(word, ',') from pg_catalog.pg_get_keywords()
  ;;   where word <> ALL ('{...}'::text[])
  ;; and passes the result through castNonNull. With the function missing
  ;; the aggregate answered ONE NULL ROW, and castNonNull raises an
  ;; AssertionError -- which Hibernate's catch(SQLException) fallback
  ;; does not catch, so every SessionFactory build failed.
  (testing "the function exists and has PG's five columns"
    (is (= [["abort" "U" "t" "unreserved" "can be bare label"]]
           (rows "SELECT * FROM pg_get_keywords() WHERE word = 'abort'"))))

  (testing "it is a real relation with many rows"
    (let [n (Long/parseLong (ffirst (rows "SELECT count(*) FROM pg_get_keywords()")))]
      (is (> n 400) (str "expected the full keyword list, got " n))))

  (testing "reachable schema-qualified, which is how pgjdbc writes it"
    (is (= (rows "SELECT count(*) FROM pg_get_keywords()")
           (rows "SELECT count(*) FROM pg_catalog.pg_get_keywords()"))))

  (testing "the pgjdbc shape yields a non-null string"
    ;; The exact exclusion list is 400+ entries; a short one exercises the
    ;; same path. What matters is that it is NOT NULL.
    (let [v (ffirst (rows (str "SELECT string_agg(word, ',') "
                               "FROM pg_catalog.pg_get_keywords() "
                               "WHERE word <> ALL ('{a,abs}'::text[])")))]
      (is (some? v))
      (is (re-find #"abort" v)))))

;; ---------------------------------------------------------------------------
;; The json/jsonb expansion family in FROM position

(deftest json-expansion-srfs-produce-rows
  ;; Every implementation already existed in datahike.pg.jsonb and was
  ;; wired for the SELECT-list path, which SERIALISES the collection into
  ;; one cell. In FROM position there was no entry at all, so the item
  ;; materialised to nothing and the query answered ZERO ROWS silently —
  ;; or, with count(*), raised the internal `Query for unknown vars`.
  (testing "array elements"
    (is (= [["1"] ["2"]] (rows "SELECT * FROM jsonb_array_elements('[1,2]'::jsonb)")))
    (is (= [["3"]] (rows "SELECT count(*) FROM jsonb_array_elements('[1,2,3]'::jsonb)")))
    (is (= [["a"] ["b"]]
           (rows "SELECT * FROM jsonb_array_elements_text('[\"a\",\"b\"]'::jsonb)"))))

  (testing "object expansion, key and value"
    (is (= [["a" "1"] ["b" "2"]]
           (rows "SELECT * FROM jsonb_each('{\"a\":1,\"b\":2}'::jsonb) ORDER BY key")))
    (is (= [["a" "1"]]
           (rows "SELECT * FROM jsonb_each_text('{\"a\":1}'::jsonb)"))))

  (testing "object keys — PG names the column after the function"
    (is (= [["a"] ["b"]]
           (rows "SELECT jsonb_object_keys FROM jsonb_object_keys('{\"a\":1,\"b\":2}'::jsonb)
                  ORDER BY 1"))))

  (testing "an empty document yields no rows, not one empty row"
    (is (= [] (rows "SELECT * FROM jsonb_array_elements('[]'::jsonb)")))
    (is (= [] (rows "SELECT * FROM jsonb_each('{}'::jsonb)")))))

(deftest json-and-jsonb-families-differ-in-punctuation
  ;; The only difference between the two spellings is how a returned
  ;; DOCUMENT is punctuated.
  (is (= [["a" "{\"x\": 1}"]]
         (rows "SELECT * FROM jsonb_each('{\"a\":{\"x\":1}}'::jsonb)")))
  (is (= [["a" "{\"x\":1}"]]
         (rows "SELECT * FROM json_each('{\"a\":{\"x\":1}}'::json)"))))

(deftest split-srfs
  (testing "regexp_split_to_table splits on a PATTERN"
    (is (= [["a"] ["b"] ["c"]] (rows "SELECT * FROM regexp_split_to_table('a1b22c', '[0-9]+')"))))
  (testing "string_to_table splits on a LITERAL — a regex would match everything"
    (is (= [["a"] ["b"] ["c"]] (rows "SELECT * FROM string_to_table('a.b.c', '.')")))))

;; ---------------------------------------------------------------------------
;; Record-shaping SRFs, whose shape comes from the AS column list

(deftest record-srfs-take-their-shape-from-the-alias
  ;; `AS r(a int, b text)` carries a TYPE per column as well as a name.
  ;; Only names were read, so these had nothing to build from and
  ;; returned no rows.
  (testing "recordset expands an array of objects"
    (is (= [["1" "x"] ["2" "y"]]
           (rows "SELECT * FROM jsonb_to_recordset('[{\"a\":1,\"b\":\"x\"},{\"a\":2,\"b\":\"y\"}]'::jsonb)
                  AS r(a int, b text) ORDER BY a"))))

  (testing "record takes a single object"
    (is (= [["1" "x"]]
           (rows "SELECT * FROM json_to_record('{\"a\":1,\"b\":\"x\"}') AS r(a int, b text)"))))

  (testing "columns are projectable and countable by name"
    (is (= [["1"] ["2"]]
           (rows "SELECT a FROM jsonb_to_recordset('[{\"a\":1},{\"a\":2}]'::jsonb)
                  AS r(a int) ORDER BY a")))
    (is (= [["2"]]
           (rows "SELECT count(*) FROM jsonb_to_recordset('[{\"a\":1},{\"a\":2}]'::jsonb)
                  AS r(a int)"))))

  (testing "a key absent from the document is NULL"
    (is (= [["1" nil]]
           (rows "SELECT * FROM jsonb_to_recordset('[{\"a\":1}]'::jsonb) AS r(a int, b text)")))))

;; ---------------------------------------------------------------------------

(deftest unknown-function-in-from-raises-42883
  ;; Used to fall through to "no relation", so `SELECT * FROM
  ;; nosuchfunc(1)` answered ZERO ROWS and count(*) raised the internal
  ;; `Query for unknown vars: [?_eid]`. PostgreSQL raises 42883.
  (let [^PgWireServer$QueryResult r (run "SELECT * FROM nosuchfunc(1)")]
    (is (re-find #"function nosuchfunc\(\) does not exist" (or (.-error r) "")))))
