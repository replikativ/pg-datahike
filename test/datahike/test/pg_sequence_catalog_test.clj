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
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.dump :as dump]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.ddl :as ddl])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)
(def ^:dynamic *conn* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*handler* (pg/make-query-handler conn)
                  *conn* conn]
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

(deftest dump-sequence-query-reflects-the-current-position
  (is (nil? (err "CREATE SEQUENCE s AS integer START 10 INCREMENT 3")))
  (is (= [] (rows "SELECT relname FROM pg_class WHERE relname = '__seq__'")))
  (let [query (str "SELECT format_type(seqtypid, NULL), last_value, is_called "
                   "FROM pg_catalog.pg_sequence, pg_get_sequence_data(seqrelid)")]
    (is (= [["integer" "10" "f"]] (rows query)))
    (is (nil? (err (str "PREPARE seq_state AS " query))))
    (is (= [["integer" "10" "f"]] (rows "EXECUTE seq_state")))
    (is (= [["42"]] (rows "SELECT setval('s', 42, false)")))
    (is (= [["integer" "42" "f"]] (rows "EXECUTE seq_state")))
    (is (= [["42" "f"]] (rows "SELECT * FROM pg_catalog.pg_get_sequence_data('s')")))
    (is (= [[nil nil]] (rows "SELECT * FROM pg_get_sequence_data(0::oid)")))
    (is (= [["42"]] (rows "SELECT nextval('s')")))
    (is (= [["42" "t"]] (rows "SELECT last_value, is_called FROM s")))
    (is (nil? (err "ALTER SEQUENCE s RESTART WITH 99")))
    (is (= [["integer" "99" "f"]] (rows query)))
    (is (= [["99"]] (rows "SELECT nextval('s')")))))

(deftest legacy-sequence-initial-state-survives-the-first-reservation
  (d/transact *conn*
              (into (vec (remove #(= :__seq__/is-called (:db/ident %)) ddl/sequence-schema))
                    [{:__seq__/name "old_seq" :__seq__/value 7
                      :__seq__/start 10 :__seq__/increment 3}]))
  (binding [*handler* (pg/make-query-handler *conn*)]
    (is (= [["10" "f"]] (rows "SELECT last_value, is_called FROM old_seq")))
    (is (= [["10" "f"]]
           (rows "SELECT last_value, is_called FROM pg_sequence, pg_get_sequence_data(seqrelid)")))
    (is (nil? (err "CREATE TABLE migration (id int DEFAULT nextval('old_seq'))")))
    (is (nil? (err "BEGIN")))
    (is (nil? (err "INSERT INTO migration DEFAULT VALUES")))
    (is (nil? (err "COMMIT")))
    (is (= [["10"]] (rows "SELECT id FROM migration")))
    (is (= [["10" "t"]] (rows "SELECT last_value, is_called FROM old_seq")))
    (is (= [["13"]] (rows "SELECT nextval('old_seq')")))))

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

(deftest postgres-rejects-invalid-sequence-owners-atomically
  (is (nil? (err "CREATE TABLE owner_table (id int)")))
  (is (nil? (err "CREATE INDEX owner_index ON owner_table (id)")))
  (is (nil? (err "CREATE MATERIALIZED VIEW owner_matview AS SELECT 1 AS id")))
  (doseq [[owner state] [["nobody" "42601"]
                         ["owner_index.oid" "42809"]
                         ["owner_matview.id" "42809"]
                         ["pg_catalog.pg_class.relname" "55000"]
                         ["owner_table.missing" "42703"]
                         ["missing_schema.owner_table.id" "3F000"]]]
    (let [sql (str "CREATE SEQUENCE rejected_seq OWNED BY " owner)]
      (is (= state (.-sqlstate ^PgWireServer$QueryResult (run sql))) sql)
      (is (= [[nil]] (rows "SELECT to_regclass('rejected_seq')")) sql)))
  (is (nil? (err "CREATE SEQUENCE rejected_seq OWNED BY owner_table.id")))
  (is (nil? (err "CREATE SEQUENCE IF NOT EXISTS rejected_seq OWNED BY nobody")))
  (is (= "42703" (.-sqlstate ^PgWireServer$QueryResult
                  (run "ALTER SEQUENCE rejected_seq OWNED BY owner_table.missing"))))
  (is (= [["public.rejected_seq"]] (rows "SELECT pg_get_serial_sequence('owner_table', 'id')")))
  (is (= [[nil]] (rows "SELECT pg_get_serial_sequence(NULL, 'id')")))
  (is (= "42703" (.-sqlstate ^PgWireServer$QueryResult
                  (run "SELECT pg_get_serial_sequence('owner_table', 'ID')"))))
  (is (= "42883" (.-sqlstate ^PgWireServer$QueryResult
                  (run "SELECT pg_get_serial_sequence(1, 2)")))))

(deftest sequence-ownership-follows-the-column-catalog
  (doseq [sql ["CREATE TABLE \"Owner Table\" (\"Col.Dot\" int, replacement int)"
               "CREATE SEQUENCE public.\"Owned.Seq\" OWNED BY public.\"Owner Table\".\"Col.Dot\""]]
    (is (nil? (err sql)) sql))
  (let [owner "SELECT pg_get_serial_sequence('public.\"Owner Table\"', 'Col.Dot')"
        owned [["public.\"Owned.Seq\""]]]
    (is (= owned (rows owner)))
    (is (= [["1" "a"]]
           (rows (str "SELECT refobjsubid, deptype FROM pg_depend "
                      "WHERE objid = '\"Owned.Seq\"'::regclass AND refobjid = '\"Owner Table\"'::regclass"))))
    (doseq [sql ["BEGIN" "ALTER SEQUENCE public.\"Owned.Seq\" OWNED BY NONE"]]
      (is (nil? (err sql)) sql))
    (is (= [[nil]] (rows owner)))
    (is (nil? (err "ROLLBACK")))
    (is (= owned (rows owner)))
    (doseq [sql ["ALTER SEQUENCE public.\"Owned.Seq\" OWNED BY public.\"Owner Table\".replacement"
                 "ALTER TABLE \"Owner Table\" RENAME COLUMN replacement TO renamed"
                 "ALTER TABLE \"Owner Table\" DROP COLUMN \"Col.Dot\""]]
      (is (nil? (err sql)) sql))
    (is (= owned (rows "SELECT pg_get_serial_sequence('\"Owner Table\"', 'renamed')")))
    (is (= [["2"]] (rows "SELECT refobjsubid FROM pg_depend WHERE objid = '\"Owned.Seq\"'::regclass")))
    (is (nil? (err "ALTER TABLE \"Owner Table\" DROP COLUMN renamed")))
    (is (= [[nil]] (rows "SELECT to_regclass('\"Owned.Seq\"')"))))
  (doseq [sql ["CREATE TABLE table_owned (id int)"
               "CREATE SEQUENCE table_owned_seq OWNED BY table_owned.id"
               "DROP TABLE table_owned"]]
    (is (nil? (err sql)) sql))
  (is (= [[nil]] (rows "SELECT to_regclass('table_owned_seq')"))))

(deftest serial-and-identity-discovery-uses-existing-ownership
  (is (nil? (err "CREATE TABLE existing_owners (a serial, b int GENERATED ALWAYS AS IDENTITY)")))
  (is (= [["public.existing_owners_a_seq" "public.existing_owners_b_seq"]]
         (rows (str "SELECT pg_get_serial_sequence('existing_owners', 'a'), "
                    "pg_get_serial_sequence('existing_owners', 'b')"))))
  (is (= "0A000" (.-sqlstate ^PgWireServer$QueryResult
                  (run "ALTER SEQUENCE existing_owners_b_seq OWNED BY NONE"))))
  (is (= "2BP01" (.-sqlstate ^PgWireServer$QueryResult
                  (run "DROP SEQUENCE existing_owners_b_seq CASCADE"))))
  (is (nil? (err "ALTER SEQUENCE existing_owners_a_seq OWNED BY NONE")))
  (is (= [[nil]] (rows "SELECT pg_get_serial_sequence('existing_owners', 'a')")))
  (is (nil? (err "ALTER TABLE existing_owners DROP COLUMN a")))
  (is (= [["1"]] (rows "SELECT nextval('existing_owners_a_seq')")))
  (is (nil? (err "ALTER TABLE existing_owners DROP COLUMN b")))
  (is (= [[nil]] (rows "SELECT to_regclass('existing_owners_b_seq')"))))

(deftest dropping-sequence-owner-handles-dependent-defaults
  (doseq [sql ["CREATE TABLE dependency_owner (id int, other int)"
               "CREATE SEQUENCE dependency_seq OWNED BY dependency_owner.id"
               (str "CREATE TABLE consumer (id bigint DEFAULT nextval('dependency_seq'), "
                    "other bigint DEFAULT nextval('dependency_seq'::regclass) + 10)")]]
    (is (nil? (err sql)) sql))
  (doseq [sql ["ALTER TABLE dependency_owner DROP COLUMN id"
               "ALTER TABLE dependency_owner DROP COLUMN id RESTRICT"
               "ALTER TABLE dependency_owner DROP id RESTRICT"
               "DROP TABLE dependency_owner"
               "DROP SEQUENCE dependency_seq"]]
    (is (= "2BP01" (.-sqlstate ^PgWireServer$QueryResult (run sql))) sql))
  (is (= [["public.dependency_seq"]]
         (rows "SELECT pg_get_serial_sequence('dependency_owner', 'id')")))
  (is (= [["1" "12"]] (rows "INSERT INTO consumer DEFAULT VALUES RETURNING id, other")))
  (is (nil? (err "ALTER TABLE dependency_owner DROP id CASCADE")))
  (is (= [[nil]] (rows "SELECT to_regclass('dependency_seq')")))
  (is (= [[nil nil]] (rows "INSERT INTO consumer DEFAULT VALUES RETURNING id, other")))
  (doseq [sql ["CREATE TABLE multi_owner (id int)"
               "CREATE SEQUENCE multi_seq OWNED BY multi_owner.id"
               "CREATE TABLE multi_consumer (id bigint DEFAULT nextval('multi_seq'))"
               "DROP TABLE multi_owner, multi_consumer"]]
    (is (nil? (err sql)) sql))
  (is (= [[nil]] (rows "SELECT to_regclass('multi_seq')")))
  (doseq [sql ["CREATE SEQUENCE direct_seq"
               "CREATE TABLE direct_consumer (id bigint DEFAULT nextval('direct_seq'))"
               "DROP SEQUENCE direct_seq CASCADE"]]
    (is (nil? (err sql)) sql))
  (is (= [[nil]] (rows "INSERT INTO direct_consumer DEFAULT VALUES RETURNING id"))))

(deftest owned-sequence-follows-its-view
  (doseq [sql ["CREATE VIEW owner_view AS SELECT 1 AS id"
               "CREATE SEQUENCE view_seq OWNED BY owner_view.id"]]
    (is (nil? (err sql)) sql))
  (is (= [["public.view_seq"]] (rows "SELECT pg_get_serial_sequence('owner_view', 'id')")))
  (is (nil? (err "DROP VIEW owner_view")))
  (is (= [[nil]] (rows "SELECT to_regclass('view_seq')"))))

(deftest qualified-sequences-keep-counters-catalogs-and-defaults-separate
  (doseq [sql ["CREATE SCHEMA billing"
               "CREATE SCHEMA \"Archive Data\""
               "CREATE SEQUENCE public.ticket START WITH 7"
               "CREATE SEQUENCE billing.ticket START WITH 101 INCREMENT BY 2"
               "CREATE SEQUENCE \"Archive Data\".\"Ticket.Name\" START WITH 901"
               "SET search_path TO billing, public"
               "CREATE TABLE billing.invoice (id bigint DEFAULT nextval('ticket'), label text)"
               "ALTER SEQUENCE billing.ticket OWNED BY billing.invoice.id"]]
    (is (nil? (err sql)) sql))
  (is (= [["101" "7" "901"]]
         (rows "SELECT nextval('ticket'), nextval('public.ticket'), nextval('\"Archive Data\".\"Ticket.Name\"')")))
  (is (= [["103"]] (rows "SELECT nextval('billing.ticket'::regclass)")))
  (is (= [["billing.ticket"]] (rows "SELECT pg_get_serial_sequence('billing.invoice','id')")))
  (is (= [["Archive Data" "Ticket.Name"] ["billing" "ticket"] ["public" "ticket"]]
         (rows "SELECT sequence_schema, sequence_name FROM information_schema.sequences ORDER BY 1,2")))
  (is (= [["t"]]
         (rows "SELECT 'billing.ticket'::regclass::oid <> 'public.ticket'::regclass::oid")))
  (is (nil? (err "SET search_path TO public")))
  (is (= [["105"]]
         (rows "INSERT INTO billing.invoice (label) VALUES ('bound default') RETURNING id")))
  (is (nil? (err "ALTER SEQUENCE billing.ticket RESTART WITH 201")))
  (is (= [["201"]]
         (rows "INSERT INTO billing.invoice (label) VALUES ('restart') RETURNING id")))
  (is (= "2BP01" (.sqlstate (run "DROP SEQUENCE billing.ticket"))))
  (is (nil? (err "DROP TABLE billing.invoice")))
  (is (= [["t" "t"]]
         (rows "SELECT to_regclass('billing.ticket') IS NULL, to_regclass('public.ticket') IS NOT NULL")))
  (is (= [["8"]] (rows "SELECT nextval('public.ticket')")))
  (is (= "42P01" (.sqlstate (run "SELECT nextval('billing.ticket')"))))
  (is (= "42P01" (.sqlstate (run "DROP SEQUENCE billing.ticket")))))

(deftest dump-style-defaults-retain-the-qualified-sequence-dependency
  (doseq [sql ["CREATE SCHEMA billing"
               "CREATE SCHEMA \"Archive Data\""
               "CREATE SEQUENCE billing.ticket START 11"
               "CREATE SEQUENCE \"Archive Data\".\"O'Brien.Ticket\" START 51"
               "SET search_path TO ''"
               "CREATE TABLE billing.invoice (id bigint DEFAULT nextval('billing.ticket'::regclass))"
               "CREATE TABLE \"Archive Data\".invoice (id bigint)"
               (str "ALTER TABLE ONLY \"Archive Data\".invoice ALTER COLUMN id "
                    "SET DEFAULT nextval('\"Archive Data\".\"O''Brien.Ticket\"'::regclass)")]]
    (is (nil? (err sql)) sql))
  (is (= [["11"]] (rows "INSERT INTO billing.invoice DEFAULT VALUES RETURNING id")))
  (is (= [["51"]] (rows "INSERT INTO \"Archive Data\".invoice DEFAULT VALUES RETURNING id")))
  (is (= [["nextval('billing.ticket'::regclass)"]]
         (rows (str "SELECT pg_catalog.pg_get_expr(adbin, adrelid) FROM pg_catalog.pg_attrdef "
                    "WHERE adrelid = 'billing.invoice'::regclass"))))
  (is (= "2BP01" (.sqlstate (run "DROP SEQUENCE billing.ticket"))))
  (is (= "2BP01" (.sqlstate (run "DROP SEQUENCE \"Archive Data\".\"O'Brien.Ticket\"")))))

(deftest generated-sequences-belong-to-their-table-namespace
  (doseq [sql ["CREATE SCHEMA billing"
               "CREATE TABLE public.invoice (serial_id serial, identity_id bigint GENERATED ALWAYS AS IDENTITY, label text)"
               "BEGIN"
               "CREATE TABLE billing.invoice (serial_id serial, identity_id bigint GENERATED ALWAYS AS IDENTITY, label text)"]]
    (is (nil? (err sql)) sql))
  (is (= [["1" "1"]] (rows "INSERT INTO billing.invoice (label) VALUES ('named') RETURNING serial_id, identity_id")))
  (is (nil? (err "COMMIT")))
  (is (= [["1" "1"]] (rows "INSERT INTO public.invoice (label) VALUES ('public') RETURNING serial_id, identity_id")))
  (is (= [["2" "2"]] (rows "INSERT INTO billing.invoice (label) VALUES ('next') RETURNING serial_id, identity_id")))
  (is (= "428C9" (.sqlstate (run "INSERT INTO billing.invoice (identity_id) VALUES (10)"))))
  (is (= [["billing.invoice_serial_id_seq" "billing.invoice_identity_id_seq"]]
         (rows (str "SELECT pg_get_serial_sequence('billing.invoice', 'serial_id'), "
                    "pg_get_serial_sequence('billing.invoice', 'identity_id')"))))
  (is (nil? (err "TRUNCATE TABLE billing.invoice RESTART IDENTITY")))
  (is (= [["1" "1"]] (rows "INSERT INTO billing.invoice DEFAULT VALUES RETURNING serial_id, identity_id")))
  (is (nil? (err "DROP TABLE billing.invoice")))
  (is (= [["t" "t"]]
         (rows (str "SELECT to_regclass('billing.invoice_identity_id_seq') IS NULL, "
                    "to_regclass('billing.invoice_serial_id_seq') IS NULL"))))
  (is (= [["2" "2"]] (rows "INSERT INTO public.invoice DEFAULT VALUES RETURNING serial_id, identity_id"))))

(deftest dump-can-attach-an-identity-sequence-to-an-existing-column
  (doseq [sql ["CREATE SCHEMA billing"
               "CREATE TABLE billing.payments (id integer NOT NULL, draft_id integer, memo text)"
               "CREATE TABLE public.payments (id integer NOT NULL, memo text)"
               "CREATE SEQUENCE public.payment_ids START WITH 400"
               "SELECT pg_catalog.set_config('search_path', '', false)"
               "BEGIN"
               (str "ALTER TABLE ONLY billing.payments ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY "
                    "(SEQUENCE NAME billing.payment_ids START WITH 7 INCREMENT BY 5 NO MINVALUE NO MAXVALUE CACHE 1)")]]
    (is (nil? (err sql)) sql))
  (is (= [["7"]] (rows "INSERT INTO billing.payments (memo) VALUES ('first') RETURNING id")))
  (is (nil? (err "COMMIT")))
  (is (= [["12"]] (rows "INSERT INTO billing.payments (memo) VALUES ('second') RETURNING id")))
  (is (= [["42"]] (rows "INSERT INTO billing.payments (id) VALUES (42) RETURNING id")))
  (is (= [["billing.payment_ids" nil]]
         (rows (str "SELECT pg_catalog.pg_get_serial_sequence('billing.payments','id'), "
                    "pg_catalog.pg_get_serial_sequence('public.payments','id')"))))
  (is (= "55000" (.sqlstate (run "ALTER TABLE billing.payments ALTER id ADD GENERATED ALWAYS AS IDENTITY"))))
  (is (= "55000" (.sqlstate (run "ALTER TABLE billing.payments ALTER draft_id ADD GENERATED ALWAYS AS IDENTITY (SEQUENCE NAME billing.draft_ids)"))))
  (is (nil? (err "DROP TABLE billing.payments")))
  (is (= [["t" "400"]]
         (rows "SELECT to_regclass('billing.payment_ids') IS NULL, nextval('public.payment_ids')"))))

(deftest table-schema-move-keeps-owned-sequences-and-existing-defaults
  (doseq [sql ["CREATE SCHEMA archive"
               "CREATE TABLE public.move_owner (a serial PRIMARY KEY, b int GENERATED BY DEFAULT AS IDENTITY)"
               "CREATE SEQUENCE public.side_counter START WITH 100"
               "CREATE TABLE public.move_consumer (
                  plain bigint DEFAULT nextval('public.move_owner_a_seq'::regclass),
                  compound bigint DEFAULT (nextval('public.move_owner_b_seq'::regclass) * 10 + nextval('public.side_counter')))"
               "CREATE SEQUENCE archive.move_owner_b_seq"]]
    (is (nil? (err sql)) sql))
  (is (= [["1" "1"]] (rows "INSERT INTO public.move_owner DEFAULT VALUES RETURNING a,b")))
  (is (= "42P07" (.sqlstate (run "ALTER TABLE public.move_owner SET SCHEMA archive"))))
  (is (nil? (err "DROP SEQUENCE archive.move_owner_b_seq")))
  (let [before (rows "SELECT 'public.move_owner_a_seq'::regclass::oid, 'public.move_owner_b_seq'::regclass::oid")]
    ;; Emulate defaults stored before DDL bound sequence references to catalog OIDs.
    (d/transact *conn* [[:db/add :move_consumer/plain :pg/default-arg "public.move_owner_a_seq"]
                        [:db/add :move_consumer/compound :pg/default-value
                         "nextval('public.move_owner_b_seq'::regclass) * 10 + nextval('public.side_counter')"]])
    (is (nil? (err "ALTER TABLE public.move_owner SET SCHEMA archive")))
    (is (= before (rows "SELECT 'archive.move_owner_a_seq'::regclass::oid, 'archive.move_owner_b_seq'::regclass::oid")))
    (is (= [[nil nil]] (rows "SELECT to_regclass('public.move_owner_a_seq'), to_regclass('public.move_owner_b_seq')")))
    (is (nil? (err "CREATE SEQUENCE public.move_owner_b_seq START WITH 700")))
    (is (= [["700"]] (rows "SELECT nextval('public.move_owner_b_seq')")))
    (is (= [["archive.move_owner_a_seq" "archive.move_owner_b_seq"]]
           (rows "SELECT pg_get_serial_sequence('archive.move_owner','a'), pg_get_serial_sequence('archive.move_owner','b')")))
    (is (= (mapv vector (first before) ["a" "i"])
           (rows "SELECT objid, deptype FROM pg_catalog.pg_depend
                  WHERE classid = 1259 AND refobjid = 'archive.move_owner'::regclass
                  ORDER BY refobjsubid")))
    (is (= [["2" "120"]]
           (rows "INSERT INTO public.move_consumer DEFAULT VALUES RETURNING plain,compound")))
    (is (= [["3" "3"]] (rows "INSERT INTO archive.move_owner DEFAULT VALUES RETURNING a,b")))
    (is (some #(re-find #"archive\.move_owner_b_seq" (first %))
              (rows "SELECT adbin FROM pg_catalog.pg_attrdef WHERE adrelid = 'public.move_consumer'::regclass")))
    (is (= "2BP01" (.sqlstate (run "DROP TABLE archive.move_owner"))))))

(deftest sequence-references-resolve-before-physical-counter-lookup
  (doseq [sql ["CREATE SCHEMA billing"
               "CREATE SEQUENCE public.ticket START 7"
               "CREATE SEQUENCE billing.ticket START 101"
               "CREATE TABLE billing.ledger (id bigint)"
               "SET search_path TO billing, public"
               "CREATE TABLE public.default_holder (id bigint DEFAULT nextval('public.ticket'))"]]
    (is (nil? (err sql)) sql))
  (let [oid (ffirst (rows "SELECT 'public.ticket'::regclass::oid"))]
    (is (nil? (err (str "CREATE SEQUENCE public.\"" oid "\" START 900"))))
    (is (= [["101"]] (rows "INSERT INTO billing.ledger VALUES (nextval('ticket')) RETURNING id")))
    (is (= [["102"]] (rows "INSERT INTO billing.ledger VALUES (nextval('billing.ticket'::regclass)) RETURNING id")))
    (is (= [["102"]] (rows "SELECT currval('ticket')")))
    (is (= [["7"]] (rows "INSERT INTO public.default_holder DEFAULT VALUES RETURNING id")))
    (is (= [["900"]] (rows (str "SELECT nextval('\"" oid "\"')"))))
    (is (nil? (err (str "DROP SEQUENCE public.\"" oid "\""))))
    (is (= [["8"]] (rows "INSERT INTO public.default_holder DEFAULT VALUES RETURNING id")))
    (testing "pre-existing stores kept the public counter name in default metadata"
      (d/transact *conn* [[:db/add :default_holder/id :pg/default-arg "ticket"]])
      (is (= [["9"]] (rows "INSERT INTO public.default_holder DEFAULT VALUES RETURNING id")))
      (d/transact *conn* [[:db/add :default_holder/id :pg/default-kind :expr]
                          [:db/add :default_holder/id :pg/default-value "100 + nextval('ticket')"]])
      (is (= [["110"]] (rows "INSERT INTO public.default_holder DEFAULT VALUES RETURNING id"))))))

(deftest generated-and-explicit-sequence-defaults-bind-independently
  (doseq [sql ["CREATE SCHEMA billing"
               "CREATE SEQUENCE billing.invoice_id_seq START 109"
               "SET search_path TO billing, public"
               (str "CREATE TABLE public.invoice (id serial, "
                    "billed bigint DEFAULT nextval('invoice_id_seq'))")
               "CREATE SEQUENCE billing.reserved_id_seq START 30"
               "CREATE TABLE billing.reserved (id serial)"
               (str "CREATE TABLE billing.generated (id serial, "
                    "offset_id bigint DEFAULT (nextval('generated_id_seq') + 10))")
               "SET search_path TO public"]]
    (is (nil? (err sql)) sql))
  (is (= [["1" "109"]] (rows "INSERT INTO public.invoice DEFAULT VALUES RETURNING id,billed")))
  (is (= [["1" "12"]] (rows "INSERT INTO billing.generated DEFAULT VALUES RETURNING id,offset_id")))
  (is (= [["billing.reserved_id_seq1"]] (rows "SELECT pg_get_serial_sequence('billing.reserved','id')")))
  (is (= [["1" "30"]] (rows "SELECT nextval('billing.reserved_id_seq1'), nextval('billing.reserved_id_seq')"))))

(deftest defaults-keep-statement-resolution-at-commit
  (doseq [sql ["CREATE SCHEMA billing"
               "CREATE SEQUENCE public.counter START 3"
               "CREATE SEQUENCE billing.counter START 12"
               "CREATE TABLE billing.ledger (id bigint)"
               "BEGIN"
               "SET search_path TO billing, public"
               "ALTER TABLE billing.ledger ALTER id SET DEFAULT nextval('counter')"
               "SET search_path TO public"
               "COMMIT"]]
    (is (nil? (err sql)) sql))
  (is (= [["12"]] (rows "INSERT INTO billing.ledger DEFAULT VALUES RETURNING id")))
  (doseq [[statement published?]
          [["CREATE TABLE candidate (id bigint DEFAULT nextval('live_counter'))" false]
           ["ALTER TABLE billing.ledger ALTER id SET DEFAULT nextval('live_counter')" true]]]
    (is (nil? (err "CREATE SEQUENCE live_counter")))
    (is (nil? (err "BEGIN")))
    (is (nil? (err statement)))
    (with-open [other (pg/make-query-handler *conn*)]
      (is (nil? (.error ^PgWireServer$QueryResult (.execute other "DROP SEQUENCE live_counter")))))
    (is (= "40001" (.sqlstate (run "COMMIT"))))
    (is (= [[nil]] (rows "SELECT to_regclass('candidate')")))
    (when published?
      (is (= [["13"]] (rows "INSERT INTO billing.ledger DEFAULT VALUES RETURNING id"))))))

(deftest schema-moves-keep-table-and-counter-storage-distinct
  (doseq [sql ["CREATE SCHEMA archive"
               "CREATE SCHEMA later"
               "CREATE TABLE public.ticket (id int)"
               "ALTER TABLE public.ticket SET SCHEMA archive"
               "CREATE SEQUENCE public.ticket START 30"
               "CREATE TABLE public.packet (id serial)"
               "ALTER TABLE public.packet SET SCHEMA archive"
               "CREATE TABLE public.packet_id_seq (id int)"
               "ALTER TABLE public.packet_id_seq SET SCHEMA later"]]
    (is (nil? (err sql)) sql))
  (is (= [["public" "ticket"]]
         (rows "SELECT sequence_schema, sequence_name FROM information_schema.sequences WHERE sequence_name='ticket'")))
  (is (= [["r" "S"]]
         (rows (str "SELECT a.relkind,p.relkind FROM pg_class a CROSS JOIN pg_class p "
                    "WHERE a.oid='archive.ticket'::regclass AND p.oid='public.ticket'::regclass"))))
  (is (= [["30" "1"]] (rows "SELECT nextval('public.ticket'), nextval('archive.packet_id_seq')")))
  (is (= [["1"]] (rows "INSERT INTO later.packet_id_seq VALUES (1) RETURNING id"))))

(deftest dump-restores-identity-sequence-persistence-option
  (doseq [sql ["CREATE SCHEMA billing"
               "CREATE TABLE billing.ledger (id int NOT NULL)"
               (str "ALTER TABLE ONLY billing.ledger ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY "
                    "(SEQUENCE NAME billing.ledger_gen UNLOGGED START WITH 50)")]]
    (is (nil? (err sql)) sql))
  (is (= [["u"]] (rows "SELECT relpersistence FROM pg_class WHERE oid='billing.ledger_gen'::regclass")))
  (is (= [["50"]] (rows "INSERT INTO billing.ledger DEFAULT VALUES RETURNING id"))))

(deftest sql-dump-rebinds-sequence-defaults-on-the-destination
  (doseq [sql ["CREATE SEQUENCE startup START 21"
               "CREATE TABLE simple_sink (id bigint DEFAULT nextval('startup'::regclass))"
               "CREATE TABLE compound_sink (id bigint DEFAULT (2 + nextval('startup'::regclass)))"]]
    (is (nil? (err sql)) sql))
  (let [statements (vec (dump/dump *conn*))]
    (fixture
     (fn []
       (is (nil? (err "CREATE TABLE unrelated (id int)")))
       (doseq [statement statements
               :when (and (not (str/blank? statement))
                          (not (str/starts-with? statement "--")))]
         (is (nil? (err statement)) statement))
       (is (= [["21"]] (rows "INSERT INTO simple_sink DEFAULT VALUES RETURNING id")))
       (is (= [["24"]] (rows "INSERT INTO compound_sink DEFAULT VALUES RETURNING id")))))))
