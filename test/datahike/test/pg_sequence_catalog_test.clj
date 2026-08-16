(ns datahike.test.pg-sequence-catalog-test
  "Issue #26 — `pg_sequences` was not populated, and sequences were
   invisible to catalog introspection generally.

   The view did not exist at all: `SELECT * FROM pg_sequences` failed
   with `XX000 Query for unknown vars`, and the filtered form answered
   zero rows. Sequences also had no `pg_class` row, so anything that
   discovers relations by walking pg_class (pg_dump, psql's \\ds, ORM
   introspection) could not see them, `SELECT * FROM myseq` raised
   42P01, and `lastval()` was an unknown function.

   All of it reads the same `:__seq__/*` entities that CREATE SEQUENCE
   writes, so the three views cannot disagree — including the
   parameters issue #21 taught CREATE SEQUENCE to store, which
   information_schema.sequences was still reporting as the bigint
   defaults.

   Expectations captured from PostgreSQL 17 by differential testing."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*handler* (pg/make-query-handler conn)]
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- run [sql] (.execute *handler* sql))

(defn- rows [sql]
  (let [^PgWireServer$QueryResult r (run sql)]
    (mapv vec (.-rows r))))

(defn- cols [sql]
  (let [^PgWireServer$QueryResult r (run sql)]
    (vec (.-columnNames r))))

(defn- err
  "The error a statement produced, or nil. `.execute` reports a failure
   on the QueryResult's `error` field (the wire layer turns it into an
   ErrorResponse) rather than by throwing."
  [sql]
  (try (.-error ^PgWireServer$QueryResult (run sql))
       (catch Exception e (ex-message e))))

;; ---------------------------------------------------------------------------
;; The reported bug
;; ---------------------------------------------------------------------------

(deftest pg-sequences-is-populated
  (testing "the setup from issue #26"
    (run "CREATE SEQUENCE bar_seq")
    (run "SELECT nextval('bar_seq')")
    (is (= [["1"]]
           (rows "SELECT last_value FROM pg_sequences WHERE sequencename = 'bar_seq'")))))

(deftest pg-sequences-full-row
  (run "CREATE SEQUENCE s1")
  (run "SELECT nextval('s1')")
  (testing "every column of PG's view, in PG's order"
    (is (= ["schemaname" "sequencename" "sequenceowner" "data_type"
            "start_value" "min_value" "max_value" "increment_by"
            "cycle" "cache_size" "last_value"]
           (cols "SELECT schemaname, sequencename, sequenceowner, data_type,
                         start_value, min_value, max_value, increment_by,
                         cycle, cache_size, last_value
                  FROM pg_sequences WHERE sequencename = 's1'")))
    (is (= [["public" "s1" "bigint" "1" "1" "9223372036854775807" "1" "f" "1" "1"]]
           (rows "SELECT schemaname, sequencename, data_type,
                         start_value, min_value, max_value, increment_by,
                         cycle, cache_size, last_value
                  FROM pg_sequences WHERE sequencename = 's1'")))))

(deftest last-value-is-null-until-first-nextval
  (testing "PG reports NULL, not the start value — the sequence has not
            handed anything out yet"
    (run "CREATE SEQUENCE s2")
    (is (= [[nil]]
           (rows "SELECT last_value FROM pg_sequences WHERE sequencename = 's2'")))
    (run "SELECT nextval('s2')")
    (is (= [["1"]]
           (rows "SELECT last_value FROM pg_sequences WHERE sequencename = 's2'")))))

(deftest last-value-is-not-served-stale-from-the-catalog-cache
  (testing "nextval changes catalog CONTENT without changing the schema,
            which the schema-hash cache key alone cannot see"
    (run "CREATE SEQUENCE s3")
    (run "SELECT nextval('s3')")
    (let [q "SELECT last_value FROM pg_sequences WHERE sequencename = 's3'"]
      (is (= [["1"]] (rows q)))
      (run "SELECT nextval('s3')")
      (is (= [["2"]] (rows q)) "same SQL text, re-run after an advance")
      (run "SELECT nextval('s3')")
      (is (= [["3"]] (rows q))))))

(deftest a-second-create-sequence-is-visible
  (testing "the first CREATE SEQUENCE installs the :__seq__ schema; later
            ones change no schema at all, so a schema-keyed cache would
            hide them"
    (run "CREATE SEQUENCE s4")
    (is (= [["s4"]] (rows "SELECT sequencename FROM pg_sequences ORDER BY sequencename")))
    (run "CREATE SEQUENCE s5")
    (is (= [["s4"] ["s5"]]
           (rows "SELECT sequencename FROM pg_sequences ORDER BY sequencename")))))

(deftest pg-sequences-reflects-create-options
  (run (str "CREATE SEQUENCE s6 START 5 INCREMENT 3 MINVALUE 2 MAXVALUE 100 "
            "CACHE 2 CYCLE AS smallint"))
  (is (= [["smallint" "5" "2" "100" "3" "t" "2"]]
         (rows "SELECT data_type, start_value, min_value, max_value,
                       increment_by, cycle, cache_size
                FROM pg_sequences WHERE sequencename = 's6'"))))

;; ---------------------------------------------------------------------------
;; The rest of the introspection surface
;; ---------------------------------------------------------------------------

(deftest sequences-appear-in-pg-class-as-relkind-S
  (run "CREATE SEQUENCE s7")
  (is (= [["s7" "S"]]
         (rows "SELECT relname, relkind FROM pg_class WHERE relname = 's7'"))))

(deftest select-star-from-a-sequence
  (testing "a sequence is a relation in PG — last_value / log_cnt / is_called"
    (run "CREATE SEQUENCE s8")
    (is (= ["last_value" "log_cnt" "is_called"]
           (cols "SELECT last_value, log_cnt, is_called FROM s8")))
    (testing "before the first advance: last_value is the start, is_called false"
      (is (= [["1" "0" "f"]] (rows "SELECT last_value, log_cnt, is_called FROM s8"))))
    (testing "after"
      (run "SELECT nextval('s8')")
      (is (= [["1" "t"]] (rows "SELECT last_value, is_called FROM s8"))))))

(deftest select-star-from-a-sequence-with-options
  (run "CREATE SEQUENCE s9 START 5 INCREMENT 3")
  (is (= [["5" "f"]] (rows "SELECT last_value, is_called FROM s9")))
  (run "SELECT nextval('s9')")
  (is (= [["5" "t"]] (rows "SELECT last_value, is_called FROM s9"))))

(deftest information-schema-sequences-honours-create-options
  (testing "was hardcoded to the bigint defaults, so a sequence created
            with START/MAXVALUE reported 1 and 2^63-1"
    (run "CREATE SEQUENCE s10 START 5 INCREMENT 3 MINVALUE 2 MAXVALUE 100")
    (is (= [["s10" "5" "2" "100" "3"]]
           (rows "SELECT sequence_name, start_value, minimum_value,
                         maximum_value, increment
                  FROM information_schema.sequences
                  WHERE sequence_name = 's10'")))))

;; ---------------------------------------------------------------------------
;; lastval()
;; ---------------------------------------------------------------------------

(deftest lastval-returns-the-last-nextval
  (run "CREATE SEQUENCE s11")
  (run "SELECT nextval('s11')")
  (is (= [["1"]] (rows "SELECT lastval()")))
  (run "SELECT nextval('s11')")
  (is (= [["2"]] (rows "SELECT lastval()"))))

(deftest lastval-tracks-the-most-recent-sequence
  (run "CREATE SEQUENCE s12")
  (run "CREATE SEQUENCE s13 START 100")
  (run "SELECT nextval('s12')")
  (run "SELECT nextval('s13')")
  (is (= [["100"]] (rows "SELECT lastval()"))
      "the sequence last advanced, not the first or the highest"))

(deftest lastval-before-any-nextval-raises
  (testing "PG raises 55000 — answering some other number would defeat
            the point of the function"
    (is (re-find #"lastval is not yet defined in this session"
                 (or (err "SELECT lastval()") "")))))

;; ---------------------------------------------------------------------------
;; Regression guard: ordinary tables must not be mistaken for sequences
;; ---------------------------------------------------------------------------

(deftest ordinary-tables-are-unaffected
  (run "CREATE TABLE t (id int PRIMARY KEY, name text)")
  (run "INSERT INTO t VALUES (1, 'a')")
  (is (= [["1" "a"]] (rows "SELECT id, name FROM t")))
  (testing "and a missing relation still raises 42P01"
    (is (re-find #"does not exist" (or (err "SELECT * FROM nope") "")))))
