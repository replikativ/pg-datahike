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
