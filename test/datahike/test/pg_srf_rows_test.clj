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
(defn- state [sql]
  (try (.-sqlstate ^PgWireServer$QueryResult (run sql))
       (catch Exception e (:sqlstate (ex-data e)))))

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

(deftest target-list-project-set-parallel-and-nested
  ;; PostgreSQL 17 tsrf.sql:6-26. SRFs at one expression depth advance in
  ;; parallel; a shorter result is NULL-padded. Nested SRFs form another
  ;; ProjectSet level, so the outer function runs once for every inner row.
  (is (= [["1" "1"] ["2" "2"] [nil "3"] [nil "4"]]
         (rows "SELECT generate_series(1,2), generate_series(1,4)")))
  (is (= [["1"] ["1"] ["2"] ["1"] ["2"] ["3"]]
         (rows "SELECT generate_series(1, generate_series(1,3))")))
  (is (= [["1" "2"] ["1" "3"] ["2" "3"]
          ["1" "4"] ["2" "4"] ["3" "4"]]
         (rows "SELECT generate_series(1, generate_series(1,3)),
                       generate_series(2,4)"))))

(deftest target-list-project-set-order-and-limit
  ;; PostgreSQL limit.sql:120-145. Base ORDER BY runs below ProjectSet and
  ;; LIMIT above it; an ORDER BY that references the SRF output moves above
  ;; ProjectSet. The argument may depend on each base row.
  (run "CREATE TABLE ps_t (id integer PRIMARY KEY, n integer)")
  (run "INSERT INTO ps_t VALUES (1,2), (2,3)")
  (is (= [["2" "1"] ["2" "2"] ["2" "3"] ["1" "1"]]
         (rows "SELECT id, generate_series(1,3) AS g
                FROM ps_t ORDER BY id DESC LIMIT 4")))
  (is (= [["1" "2"] ["1" "1"] ["2" "3"] ["2" "2"] ["2" "1"]]
         (rows "SELECT id, generate_series(1,n) AS g
                FROM ps_t ORDER BY id, g DESC")))
  (is (= [["1" "3"] ["1" "2"] ["1" "1"]
          ["2" "3"] ["2" "2"] ["2" "1"]]
         (rows "SELECT id, generate_series(1,3) AS g
                FROM ps_t ORDER BY id, generate_series(1,3) DESC")))
  (is (= [["2"] ["1"] ["0"]]
         (rows "SELECT generate_series(0,2) AS s
                ORDER BY s DESC")))
  (is (= 3
         (count
          (rows "SELECT unnest(array_fill('2020-01-02 03:04:05'::timestamp,
                                          ARRAY[3]))")))))

(deftest target-list-project-set-respects-empty-base-relation
  (run "CREATE TABLE ps_empty (id integer PRIMARY KEY)")
  (run "INSERT INTO ps_empty VALUES (1)")
  (is (= [] (rows "SELECT generate_series(1,3) FROM ps_empty WHERE false"))))

(deftest project-set-rejects-unsupported-stage-orderings
  (testing "window functions must see base rows before ProjectSet expansion"
    (is (= "0A000"
           (state "SELECT row_number() OVER (), generate_series(1,2)"))))
  (testing "DISTINCT ON can sit on either side of ProjectSet depending on its keys"
    (is (= "0A000"
           (state "SELECT DISTINCT ON (generate_series(1,2))
                          generate_series(1,2)")))))

(deftest project-set-is-consumed-by-data-producing-statements
  ;; ProjectSet is a logical executor stage. Statements which execute a
  ;; translated SELECT directly must materialise it before trimming the
  ;; hidden SRF argument columns or persisting the rows.
  (run "CREATE TABLE ps_insert (id integer PRIMARY KEY)")
  (is (= "INSERT 0 3"
         (.-commandTag ^PgWireServer$QueryResult
          (run "INSERT INTO ps_insert SELECT generate_series(1,3)"))))
  (is (= [["1"] ["2"] ["3"]]
         (rows "SELECT id FROM ps_insert ORDER BY id")))

  (run "CREATE TABLE ps_ctas AS SELECT generate_series(1,3) AS id")
  (is (= [["1"] ["2"] ["3"]]
         (rows "SELECT id FROM ps_ctas ORDER BY id")))

  (is (= [["1"] ["2"] ["3"] ["4"]]
         (rows "SELECT generate_series(1,2) AS id
                UNION ALL
                SELECT generate_series(3,4) AS id
                ORDER BY id"))))

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

(deftest record-srf-temporal-and-narrow-int-columns
  ;; The declared type has to reach BOTH the value and the catalog. A
  ;; `date` coldef casts to a LocalDate, but the row is transacted into
  ;; a :db.type/instant attribute that only accepts a java.util.Date —
  ;; without normalising, the transaction rejected it outright.
  (testing "a date column round-trips as a date"
    (is (= [["2020-01-01"]]
           (rows "SELECT * FROM jsonb_to_recordset('[{\"d\":\"2020-01-01\"}]'::jsonb)
                  AS r(d date)"))))
  (testing "a narrow integer keeps its declared width"
    (is (= [["1"]]
           (rows "SELECT * FROM jsonb_to_recordset('[{\"a\":1}]'::jsonb) AS r(a smallint)")))))
