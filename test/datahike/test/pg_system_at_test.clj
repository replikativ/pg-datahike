(ns datahike.test.pg-system-at-test
  "Tests for the tx-time (SQL:2011 SYSTEM_TIME) SQL session var:
   `datahike.system_at`, a SQL:2011-compliant alias for the existing
   `datahike.as_of` var.

   Both names map to the same internal `:as-of` session-state key
   and route through `d/as-of` on the db. The dual surface lets
   pg-datahike speak SQL:2011 (`SYSTEM_TIME`) while keeping the
   datahike-native naming (`as_of`) for existing consumers.

   We test:
   - parse-temporal-set recognises both names and maps to :as-of
   - apply-temporal threads the parsed Date into d/as-of
   - per-statement override :as-of shadows session-state
   - per-statement override :as-of :all clears any session pin
   - end-to-end through the handler: SET / SELECT / RESET cycle
     completes without error."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.schema :as pgs]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:private parse-temporal-set
  (resolve 'datahike.pg.server/parse-temporal-set))

(def ^:private apply-temporal
  (resolve 'datahike.pg.server/apply-temporal))

(def test-schema
  [{:db/ident :person/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :person/age
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}])

(def ^:dynamic *conn* nil)
(def ^:dynamic *handler* nil)

(defn pg-fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          _ (d/transact conn test-schema)
          _ (d/transact conn [{:person/name "Alice" :person/age 30}
                              {:person/name "Bob"   :person/age 25}])
          handler (pg/make-query-handler conn)]
      (try
        (binding [*conn* conn
                  *handler* handler]
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each pg-fixture)

(defn- err [^PgWireServer$QueryResult r] (.error r))

;; ============================================================================
;; parse-temporal-set

(deftest parse-temporal-set-recognises-system-at
  (testing "datahike.system_at maps to :as-of"
    (is (= [:as-of "2024-04-15T00:00:00Z"]
           (@parse-temporal-set "SET datahike.system_at = '2024-04-15T00:00:00Z'"))))
  (testing "datahike.as_of still maps to :as-of (legacy name)"
    (is (= [:as-of "2024-04-15T00:00:00Z"]
           (@parse-temporal-set "SET datahike.as_of = '2024-04-15T00:00:00Z'"))))
  (testing "RESET datahike.system_at clears via :as-of"
    (is (= [:as-of nil]
           (@parse-temporal-set "RESET datahike.system_at"))))
  (testing "SET '' clears the var"
    (is (= [:as-of nil]
           (@parse-temporal-set "SET datahike.system_at = ''")))))

;; ============================================================================
;; apply-temporal threads as-of through d/as-of

(deftest apply-temporal-marks-db-with-as-of
  (let [tx-at (java.util.Date.)
        session (atom {:as-of tx-at})
        marked (@apply-temporal (d/db *conn*) session)]
    (testing "session :as-of produces an AsOfDB wrapper"
      (is (instance? datahike.db.AsOfDB marked)))))

(deftest apply-temporal-no-as-of-wrapper-when-unset
  (let [session (atom {})
        plain (@apply-temporal (d/db *conn*) session)]
    (testing "no :as-of in session-state → no AsOfDB wrapper"
      (is (not (instance? datahike.db.AsOfDB plain))))))

;; ============================================================================
;; per-statement override shadows session

(deftest apply-temporal-override-as-of-shadows-session
  (let [session-at (java.util.Date. 1000000)
        override-at (java.util.Date. 2000000)
        session (atom {:as-of session-at})
        marked (@apply-temporal (d/db *conn*) session {:as-of override-at})]
    (testing "per-statement :as-of override replaces session :as-of"
      (is (instance? datahike.db.AsOfDB marked))
      ;; AsOfDB has a `time-point` field — verify it picked the override
      (is (= override-at (.-time-point ^datahike.db.AsOfDB marked))))))

(deftest apply-temporal-override-as-of-all-clears-session
  (let [session-at (java.util.Date.)
        session (atom {:as-of session-at})
        plain (@apply-temporal (d/db *conn*) session {:as-of :all})]
    (testing "per-statement :as-of :all clears any session pin for this stmt"
      (is (not (instance? datahike.db.AsOfDB plain)))
      (is (= session-at (:as-of @session))
          "session-state untouched — :as-of pin restored for next stmt"))))

;; ============================================================================
;; End-to-end through the handler

(deftest set-system-at-survives-roundtrip
  (testing "SET datahike.system_at = '<iso-instant>' completes without error"
    (let [r (.execute *handler* "SET datahike.system_at = '2099-01-01T00:00:00Z'")]
      (is (nil? (err r)))))
  (testing "Subsequent SELECT under system_at runs without error"
    (let [r (.execute *handler* "SELECT name, age FROM person ORDER BY age")]
      (is (nil? (err r)))))
  (testing "RESET datahike.system_at clears"
    (let [r (.execute *handler* "RESET datahike.system_at")]
      (is (nil? (err r))))))

(deftest column-type-rewrites-preserve-rows-and-catalog-addresses
  (let [run #(.execute *handler* %)
        rows (fn [sql]
               (let [result (run sql)]
                 (is (nil? (err result)) sql)
                 (mapv vec (.rows result))))]
    (doseq [sql ["SET TIME ZONE 'UTC'"
                 (str "CREATE TABLE audit_events (id bigint PRIMARY KEY, raw varchar, "
                      "created_at timestamp DEFAULT '2024-02-29 13:55:19.123456')")
                 "INSERT INTO audit_events (id, raw) VALUES (1, ' 23 ')"
                 "CREATE INDEX audit_raw_idx ON audit_events (raw)"]]
      (is (nil? (err (run sql))) sql))
    (let [before (d/db *conn*)
          snapshot (.toInstant ^java.util.Date (:db/txInstant (d/entity before (:max-tx before))))]
      (is (= "42804" (.sqlstate (run "ALTER TABLE audit_events ALTER raw TYPE integer"))))
      (is (= "22001" (.sqlstate (run "ALTER TABLE audit_events ALTER raw TYPE varchar(2)"))))
      (is (= [["1" " 23 " "2024-02-29 13:55:19.123456"]]
             (rows "SELECT * FROM audit_events")))
      (is (nil? (err (run "ALTER TABLE audit_events ALTER created_at TYPE timestamp(3)"))))
      (is (nil? (err (run (str "ALTER TABLE audit_events ALTER COLUMN raw SET DATA TYPE integer "
                               "USING trim(raw)::int + 1, ALTER COLUMN raw SET NOT NULL")))))
      (is (= [["1" "24" "2024-02-29 13:55:19.123"]]
             (rows "SELECT * FROM audit_events")))
      (is (= [["id" "20" "-1"] ["raw" "23" "-1"] ["created_at" "1114" "3"]]
             (rows (str "SELECT attname, atttypid, atttypmod FROM pg_attribute "
                        "WHERE attrelid = 'audit_events'::regclass AND attnum > 0 ORDER BY attnum"))))
      (is (= [["1" "24"]]
             (rows "SELECT id, raw FROM audit_events WHERE raw = 24")))
      (let [parsed (.parse *handler* "SELECT id FROM audit_events WHERE raw = $1" (int-array 0))]
        (is (= [23] (vec (.describeParams *handler* parsed)))))
      (is (= [["2"]]
             (rows "SELECT indkey::text FROM pg_index WHERE indexrelid = 'audit_raw_idx'::regclass")))
      (is (= [["2" "26" "2024-02-29 13:55:19.123"]]
             (rows "INSERT INTO audit_events (id, raw) VALUES (2, 26) RETURNING id, raw, created_at")))
      (is (= "23502" (.sqlstate (run "INSERT INTO audit_events (id) VALUES (3)"))))
      (is (nil? (err (run (str "SET datahike.as_of = '" snapshot "'")))))
      (is (= [["1" " 23 " "2024-02-29 13:55:19.123456"]]
             (rows "SELECT * FROM audit_events"))))))

(deftest column-type-rewrite-rolls-back-and-allows-dropping-an-old-default
  (let [run #(.execute *handler* %)]
    (doseq [sql ["CREATE TABLE tasks (key integer PRIMARY KEY, detail text DEFAULT 'old')"
                 "INSERT INTO tasks (key) VALUES (1)"
                 "BEGIN"
                 (str "ALTER TABLE tasks ALTER COLUMN detail DROP DEFAULT, "
                      "ALTER COLUMN detail TYPE integer USING length(detail)")]]
      (is (nil? (err (run sql))) sql))
    (is (= [["3"]] (mapv vec (.rows (run "SELECT detail FROM tasks")))))
    (is (nil? (err (run "ROLLBACK"))))
    (is (= [["old"]] (mapv vec (.rows (run "SELECT detail FROM tasks")))))
    (is (= [["old"]]
           (mapv vec (.rows (run "INSERT INTO tasks (key) VALUES (2) RETURNING detail")))))
    (doseq [sql ["BEGIN"
                 (str "ALTER TABLE tasks ALTER COLUMN detail DROP DEFAULT, "
                      "ALTER COLUMN detail TYPE integer USING length(detail)")
                 "UPDATE tasks SET detail = detail + 1 WHERE key = 2"
                 "COMMIT"]]
      (is (nil? (err (run sql))) sql))
    (is (= [["3"] ["4"]] (mapv vec (.rows (run "SELECT detail FROM tasks ORDER BY key")))))))

(deftest writes-resolve-the-logical-table-name
  (let [run #(.execute *handler* %)
        data #(mapv vec (.rows (run %)))]
    (is (nil? (err (run "CREATE TABLE physical_name (id int PRIMARY KEY, amount int)"))))
    (let [db (d/db *conn*)
          oid (pgs/table-oid db "physical_name")]
      (d/transact *conn* (objects/rename-table-tx db oid "visible_name")))
    (is (= [["1" "10"]]
           (data "INSERT INTO visible_name VALUES (1, 10) RETURNING visible_name.*")))
    (is (= [["20"]]
           (data (str "UPDATE visible_name SET amount = src.amount "
                      "FROM (VALUES (1, 20)) AS src(id,amount) "
                      "WHERE visible_name.id = src.id RETURNING visible_name.amount"))))
    (is (= [["20"]] (data "SELECT amount FROM visible_name")))
    (is (= [["20"]]
           (data "DELETE FROM visible_name WHERE visible_name.id = 1 RETURNING visible_name.amount")))
    (is (= [["0"]] (data "SELECT count(*) FROM visible_name")))
    (is (.-copyInMode (run "COPY public.visible_name FROM STDIN")))
    (.copyChunk *handler* (.getBytes "2\t30\n" java.nio.charset.StandardCharsets/UTF_8))
    (is (nil? (err (.copyComplete *handler*))))
    (is (= ["2\t30"] (vec (.-copyOutLines (run "COPY visible_name TO STDOUT")))))
    (doseq [sql ["INSERT INTO physical_name VALUES (2, 30)"
                 "UPDATE physical_name SET amount = 30"
                 "DELETE FROM physical_name"
                 "COPY physical_name FROM STDIN"
                 "COPY physical_name TO STDOUT"]]
      (is (= "42P01" (.sqlstate (run sql))) sql))))

(deftest dropped-table-remains-readable-only-in-its-own-snapshot
  (let [run #(.execute *handler* %)
        execute (fn [sql] (is (nil? (err (run sql))) sql))
        data #(mapv vec (.rows (run %)))
        pin (fn [instant] (execute (str "SET datahike.as_of = '" instant "'")))
        snapshot (fn []
                   (let [db (d/db *conn*)]
                     (.toInstant ^java.util.Date (:db/txInstant (d/entity db (:max-tx db))))))]
    (execute "CREATE TABLE ledger (id int PRIMARY KEY, amount int)")
    (execute "INSERT INTO ledger VALUES (1, 7)")
    (let [before (snapshot)
          old-oid (pgs/table-oid (d/db *conn*) "ledger")]
      (execute "DROP TABLE ledger")
      (let [between (snapshot)]
        (is (= [[nil]] (data "SELECT to_regclass('ledger')")))
        (is (= "42P01" (.sqlstate (run "SELECT * FROM ledger"))))
        (is (= "42P01" (.sqlstate (run "ALTER TABLE ledger ADD COLUMN retired int"))))
        (execute "DROP TABLE IF EXISTS ledger")
        (execute "CREATE TABLE ledger (id int PRIMARY KEY, amount text)")
        (is (not= old-oid (pgs/table-oid (d/db *conn*) "ledger")))
        (execute "ALTER TABLE ledger ADD COLUMN note text DEFAULT 'new'")
        (execute "CREATE INDEX ledger_amount_idx ON ledger (amount)")
        (is (= [["CREATE INDEX ledger_amount_idx ON public.ledger USING btree (amount)"]]
               (data "SELECT pg_get_indexdef(indexrelid) FROM pg_index WHERE indexrelid = 'ledger_amount_idx'::regclass")))
        (is (= [["ledger_id_not_null"] ["ledger_pkey"]]
               (data "SELECT conname FROM pg_constraint WHERE conrelid = 'ledger'::regclass ORDER BY conname")))
        (execute "INSERT INTO ledger (id, amount) VALUES (1, 'seven')")
        (is (= [["1" "seven" "new"]] (data "SELECT id, amount, note FROM ledger")))
        (pin before)
        (is (= [["1" "7"]] (data "SELECT * FROM ledger")))
        (pin between)
        (is (= "42P01" (.sqlstate (run "SELECT * FROM ledger"))))
        (execute "RESET datahike.as_of")
        (execute "TRUNCATE ledger")
        (is (= [["0"]] (data "SELECT count(*) FROM ledger")))
        (execute "DROP TABLE ledger")
        (is (= [[nil]] (data "SELECT to_regclass('ledger')")))))))

(deftest table-retirement-and-recreation-are-transactional
  (let [run #(.execute *handler* %)
        execute (fn [sql] (is (nil? (err (run sql))) sql))
        data #(mapv vec (.rows (run %)))]
    (execute "CREATE TABLE ledger (id int)")
    (execute "INSERT INTO ledger VALUES (1)")
    (doseq [finish ["ROLLBACK" "COMMIT"]]
      (execute "BEGIN")
      (execute "DROP TABLE ledger")
      (is (= [[nil]] (data "SELECT to_regclass('ledger')")))
      (execute "CREATE TABLE ledger AS SELECT 'current'::text AS id")
      (is (= [["current"]] (data "SELECT id FROM ledger")))
      (execute finish)
      (is (= (if (= "COMMIT" finish) [["current"]] [["1"]])
             (data "SELECT id FROM ledger"))))))

(deftest recreated-table-dependents-follow-the-current-generation
  (let [run #(.execute *handler* %)
        execute (fn [sql] (is (nil? (err (run sql))) sql))
        data #(mapv vec (.rows (run %)))]
    (doseq [sql ["CREATE TABLE ledger (id serial PRIMARY KEY)"
                 "INSERT INTO ledger DEFAULT VALUES"
                 "DROP TABLE ledger"
                 "CREATE TABLE ledger (id int GENERATED ALWAYS AS IDENTITY PRIMARY KEY, amount int CHECK (amount > 0))"
                 "CREATE TABLE fired (relation_name text)"
                 (str "CREATE FUNCTION remember_table() RETURNS trigger LANGUAGE plpgsql AS "
                      "$$ BEGIN INSERT INTO fired VALUES (TG_TABLE_NAME); RETURN NEW; END; $$")
                 "CREATE TRIGGER remember AFTER INSERT ON ledger FOR EACH ROW EXECUTE FUNCTION remember_table()"
                 "CREATE TABLE entry (item int REFERENCES ledger)"
                 "INSERT INTO ledger (amount) VALUES (5)"
                 "DROP TRIGGER remember ON ledger"
                 "INSERT INTO entry VALUES (1)"]]
      (execute sql))
    (is (= [["ledger"]] (data "SELECT relation_name FROM fired")))
    (is (= [["public.ledger_id_seq"]] (data "SELECT pg_get_serial_sequence('ledger', 'id')")))
    (is (= [["a"]] (data (str "SELECT attidentity FROM pg_attribute "
                              "WHERE attrelid = 'ledger'::regclass AND attname = 'id'"))))
    (is (= "428C9" (.sqlstate (run "INSERT INTO ledger VALUES (2, 9)"))))
    (is (= "23514" (.sqlstate (run "INSERT INTO ledger (amount) VALUES (-1)"))))
    (is (= "23503" (.sqlstate (run "INSERT INTO entry VALUES (99)"))))
    (is (= "2BP01" (.sqlstate (run "DROP TABLE ledger"))))
    (execute "DROP TABLE entry")
    (execute "TRUNCATE ledger RESTART IDENTITY")
    (is (= [["1"]] (data "INSERT INTO ledger (amount) VALUES (8) RETURNING id")))
    (execute "DROP TABLE ledger")
    (is (= [[nil nil]] (data "SELECT to_regclass('ledger'), to_regclass('ledger_id_seq')")))))

(deftest table-retirement-removes-rows-inserted-after-planning
  (is (nil? (err (.execute *handler* "CREATE TABLE ledger (id int)"))))
  (is (nil? (err (.execute *handler* "INSERT INTO ledger VALUES (1)"))))
  (with-open [writer (pg/make-query-handler *conn*)]
    (let [plan (resolve 'datahike.pg.server/drop-table-tx-data)
          original @plan
          snapshot (atom nil)]
      (with-redefs-fn
        {plan (fn [& args]
                (let [tx-data (apply original args)]
                  (is (nil? (err (.execute writer "INSERT INTO ledger VALUES (2)"))))
                  (reset! snapshot (:max-tx (d/db *conn*)))
                  tx-data))}
        #(is (nil? (err (.execute *handler* "DROP TABLE ledger")))))
      (let [db (d/db *conn*)]
        (is (empty? (d/datoms db :aevt :ledger/id)))
        (is (= #{1 2} (into #{} (map :v) (d/datoms (d/as-of db @snapshot) :aevt :ledger/id))))))))

(deftest historical-catalogs-and-user-data-share-the-statement-snapshot
  (let [run #(.execute *handler* %)
        rows (fn [query]
               (let [result (run query)]
                 (is (nil? (err result)) (str query " " (err result)))
                 (mapv vec (.rows result))))
        columns (str "SELECT column_name, data_type, datetime_precision FROM information_schema.columns "
                     "WHERE table_name = 'temporal_example' AND column_name <> 'db_id' ORDER BY ordinal_position")]
    (doseq [query ["CREATE TABLE temporal_example (id integer PRIMARY KEY, label text, obsolete text, stamp timestamp(6))"
                   "INSERT INTO temporal_example VALUES (1, 'before', 'old', '2024-02-29 12:00:00.123456')"]]
      (is (nil? (err (run query))) query))
    (let [before (d/db *conn*)
          snapshot (.toInstant ^java.util.Date (:db/txInstant (d/entity before (:max-tx before))))]
      (doseq [query ["ALTER TABLE temporal_example RENAME COLUMN label TO title"
                     "ALTER TABLE temporal_example DROP COLUMN obsolete, ADD COLUMN extra bigint"
                     "ALTER TABLE temporal_example ALTER COLUMN stamp TYPE timestamp(3)"
                     "UPDATE temporal_example SET title = 'after'"
                     "CREATE TABLE future_example (id integer PRIMARY KEY)"
                     (str "SET datahike.as_of = " (:max-tx before))]]
        (is (nil? (err (run query))) query))
      (is (= [["id" "integer" nil] ["label" "text" nil] ["obsolete" "text" nil]
              ["stamp" "timestamp without time zone" "6"]]
             (rows columns)))
      (is (= [["label"] ["obsolete"]]
             (rows (str "SELECT a.attname FROM pg_attribute a JOIN pg_class r ON a.attrelid = r.oid "
                        "WHERE r.relname = 'temporal_example' AND a.attname IN ('label', 'obsolete') ORDER BY a.attnum"))))
      (is (empty? (rows "SELECT relname FROM pg_class WHERE relname = 'future_example'")))
      (is (empty? (rows "SELECT table_name FROM information_schema.tables WHERE table_name = 'future_example'")))
      (is (= [["before" "label"]]
             (rows (str "SELECT t.label, c.column_name FROM temporal_example t "
                        "JOIN information_schema.columns c ON c.table_name = 'temporal_example' "
                        "AND c.column_name = 'label' WHERE t.id = 1"))))
      (is (= [["before" "label"]]
             (rows (str "WITH cols AS (SELECT column_name FROM information_schema.columns "
                        "WHERE table_name = 'temporal_example' AND column_name = 'label') "
                        "SELECT t.label, c.column_name FROM temporal_example t CROSS JOIN cols c WHERE t.id = 1"))))
      (is (nil? (err (run "RESET datahike.as_of"))))
      (is (= [["id" "integer" nil] ["title" "text" nil] ["stamp" "timestamp without time zone" "3"]
              ["extra" "bigint" nil]]
             (rows columns)))
      (let [parsed (.parse *handler*
                           (str "SELECT label, stamp FROM temporal_example FOR SYSTEM_TIME AS OF '"
                                snapshot "' WHERE id = $1")
                           (int-array 0))
            described (.describeResult *handler* parsed)
            result (.executePrepared *handler* parsed (object-array [nil 1]))]
        (is (= [23] (vec (.describeParams *handler* parsed))))
        (is (= [25 1114] (vec (.-columnOids described))))
        (is (nil? (err result)))
        (is (= [["before" "2024-02-29 12:00:00.123456"]] (mapv vec (.rows result)))))
      (is (= [["after"]] (rows "SELECT title FROM temporal_example WHERE id = 1"))))))

(deftest renamed-relations-resolve-against-the-statement-snapshot
  (let [run #(.execute *handler* %)
        rows (fn [query]
               (let [result (run query)]
                 (is (nil? (err result)) (str query " " (err result)))
                 (mapv vec (.rows result))))
        rename! (fn [old new]
                  (let [db (d/db *conn*)]
                    (d/transact *conn* (objects/rename-table-tx db (pgs/table-oid db old) new))))]
    (doseq [query ["CREATE TABLE rename_original (id integer PRIMARY KEY, caption text)"
                   "INSERT INTO rename_original VALUES (1, 'old row'), (2, 'second row')"]]
      (is (nil? (err (run query))) query))
    (let [old-db (d/db *conn*)
          oid (str (pgs/table-oid old-db "rename_original"))]
      (is (nil? (err (run "ALTER TABLE rename_original RENAME COLUMN caption TO label"))))
      (rename! "rename_original" "rename_archive")
      (is (= "42P01" (.sqlstate (run "SELECT * FROM rename_original"))))
      (is (= [["1" "old row"] ["2" "second row"]]
             (rows "SELECT id, label FROM public.rename_archive ORDER BY id")))
      (doseq [query ["CREATE TABLE rename_replacement (id integer PRIMARY KEY, code bigint)"
                     "INSERT INTO rename_replacement VALUES (1, 42)"]]
        (is (nil? (err (run query))) query))
      (rename! "rename_replacement" "rename_original")
      (is (= [["old row" "42"]]
             (rows (str "SELECT a.label, r.code FROM rename_archive a "
                        "JOIN rename_original r ON a.id = r.id"))))
      (is (= [[oid "rename_archive" "rename_archive"]]
             (rows (str "SELECT c.oid, c.relname, t.typname FROM pg_class c "
                        "JOIN pg_type t ON c.reltype = t.oid WHERE c.oid = 'rename_archive'::regclass"))))
      (is (= [["rename_archive" "label"] ["rename_original" "code"]]
             (rows (str "SELECT table_name, column_name FROM information_schema.columns "
                        "WHERE table_name IN ('rename_archive', 'rename_original') "
                        "AND column_name IN ('label', 'code') ORDER BY table_name"))))
      (is (nil? (err (run (str "SET datahike.as_of = " (:max-tx old-db))))))
      (is (= [["1" "old row"] ["2" "second row"]] (rows "SELECT * FROM rename_original ORDER BY id")))
      (is (= "42P01" (.sqlstate (run "SELECT * FROM rename_archive"))))
      (is (= [[oid "rename_original"]]
             (rows "SELECT oid, relname FROM pg_class WHERE oid = 'rename_original'::regclass"))))))
