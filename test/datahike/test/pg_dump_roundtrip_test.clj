(ns datahike.test.pg-dump-roundtrip-test
  "Self-loop roundtrip: pg-datahike → dump → fresh pg-datahike →
   verify identical rows. Stresses the schema emit (CREATE TABLE +
   CREATE SEQUENCE + ALTER ADD CONSTRAINT) and the data emit (both
   INSERT and COPY formats) on a multi-table fixture with FKs,
   sequences, mixed types, NULLs, and string-escaping edge cases."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.dump :as dump])
  (:import [java.io StringReader]
           [java.sql Connection DriverManager]
           [org.postgresql PGConnection]
           [org.postgresql.copy CopyManager]))

(def ^:dynamic *src-conn* nil)
(def ^:dynamic *src-port* nil)
(def ^:dynamic *src-cfg* nil)

(defn- jdbc-url [port db]
  (str "jdbc:postgresql://localhost:" port "/" db
       "?user=datahike&password=datahike"))

(defn- src-fixture [f]
  (Class/forName "org.postgresql.Driver")
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          srv  (pg/start-server conn {:port 0})
          port (.getPort ^datahike.pg.PgWireServer (:server srv))]
      (try
        (binding [*src-conn* conn *src-port* port *src-cfg* cfg]
          (f))
        (finally
          (.stop ^datahike.pg.PgWireServer (:server srv))
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each src-fixture)

(defn- exec! [port db ^String sql]
  (with-open [c (DriverManager/getConnection (jdbc-url port db))
              stmt (.createStatement c)]
    (.execute stmt sql)))

(defn- query-all [port db ^String sql]
  (with-open [c (DriverManager/getConnection (jdbc-url port db))
              stmt (.createStatement c)
              rs   (.executeQuery stmt sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [out []]
        (if (.next rs)
          (recur (conj out (mapv #(.getObject rs (int %)) (range 1 (inc n)))))
          out)))))

(defn- with-target
  "Spin up a fresh pg-datahike server, run BODY-FN with [port db].
   Tears down on exit."
  [body-fn]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          srv  (pg/start-server conn {:port 0})
          port (.getPort ^datahike.pg.PgWireServer (:server srv))]
      (try
        (body-fn port "datahike")
        (finally
          (.stop ^datahike.pg.PgWireServer (:server srv))
          (d/release conn)
          (d/delete-database cfg))))))

(defn- find-copy-block
  "Locate the first COPY ... FROM stdin block in SQL. Returns
   {:before pre :sql header :body body :after post} or nil."
  [^String sql]
  ;; Header: COPY ... FROM stdin;\n   Body: lines until a sole `\.\n`
  (when-let [m (re-find #"(?ms)(COPY\s+\"[^\"]+\"\s+\([^)]+\)\s+FROM\s+stdin)\s*;\s*\n" sql)]
    (let [[hdr-full hdr-sql] m
          start  (.indexOf sql ^String hdr-full)
          body-start (+ start (.length ^String hdr-full))
          eod    (.indexOf sql "\n\\.\n" (int body-start))
          eod-or-end (if (neg? eod)
                       (let [tail (.indexOf sql "\n\\." (int body-start))]
                         (if (neg? tail) (.length sql) tail))
                       eod)
          body  (subs sql body-start eod-or-end)
          ;; Skip past the EOD line ("\n\\.\n" → 4 chars; "\n\\." trailing → 3)
          after-start (cond
                        (>= eod 0) (+ eod 4)
                        :else (.length sql))]
      {:before (subs sql 0 start)
       :sql    hdr-sql
       :body   body
       :after  (if (>= after-start (.length sql)) "" (subs sql after-start))})))

(defn- replay-dump!
  "Replay a dump's full SQL text into the target server. Walks the
   SQL forward, executing non-COPY chunks via `Statement.execute`
   and driving COPY-IN blocks via pgjdbc's `CopyManager`. This
   mirrors what `psql` does under the hood, so the replayer
   accepts both `--inserts` and `--copy` dump formats."
  [^String sql port db]
  (with-open [c (DriverManager/getConnection (jdbc-url port db))]
    (let [pgconn (.unwrap c PGConnection)
          cm     (CopyManager. pgconn)]
      (loop [rest-sql sql]
        (if-let [block (find-copy-block rest-sql)]
          (let [{:keys [before sql body after]} block]
            (when (seq (str/trim before))
              (with-open [stmt (.createStatement c)]
                (.execute stmt before)))
            (.copyIn cm ^String sql (StringReader. ^String body))
            (recur after))
          (when (seq (str/trim rest-sql))
            (with-open [stmt (.createStatement c)]
              (.execute stmt rest-sql))))))))

(defn- roundtrip-rows
  "Run BUILD-FIXTURE-FN on the source, dump, replay into a fresh
   target, and return [src-rows tgt-rows] as fetched by SELECT-SQL.
   FORMAT is :inserts or :copy."
  [build-fixture-fn select-sql format]
  (build-fixture-fn)
  (let [src-rows (query-all *src-port* "datahike" select-sql)
        sql      (dump/dump-to-string *src-conn* {:format format})
        tgt-rows (atom nil)]
    (with-target
      (fn [tport tdb]
        (replay-dump! sql tport tdb)
        (reset! tgt-rows (query-all tport tdb select-sql))))
    [src-rows @tgt-rows]))

;; ============================================================================
;; Multi-table with FK
;; ============================================================================

(deftest roundtrip-multi-table-fk-inserts
  (let [build (fn []
                (exec! *src-port* "datahike"
                       "CREATE TABLE author (id INTEGER PRIMARY KEY, name TEXT)")
                (exec! *src-port* "datahike"
                       (str "CREATE TABLE book ("
                            "id INTEGER PRIMARY KEY, "
                            "author_id INTEGER REFERENCES author(id), "
                            "title TEXT)"))
                (exec! *src-port* "datahike" "INSERT INTO author VALUES (1, 'borges')")
                (exec! *src-port* "datahike" "INSERT INTO author VALUES (2, 'calvino')")
                (exec! *src-port* "datahike" "INSERT INTO book VALUES (10, 1, 'Ficciones')")
                (exec! *src-port* "datahike" "INSERT INTO book VALUES (20, 2, 'Invisible Cities')"))]
    (let [[src tgt] (roundtrip-rows build "SELECT id, name FROM author ORDER BY id" :inserts)]
      (is (= src tgt) "author rows match"))
    ;; book table on a fresh fixture (deftest reruns full fixture; skip extra exec by
    ;; just re-asserting on the same dump in a single fixture pass)
    ))

(deftest roundtrip-multi-table-book-rows
  (exec! *src-port* "datahike"
         "CREATE TABLE author (id INTEGER PRIMARY KEY, name TEXT)")
  (exec! *src-port* "datahike"
         (str "CREATE TABLE book ("
              "id INTEGER PRIMARY KEY, "
              "author_id INTEGER REFERENCES author(id), "
              "title TEXT)"))
  (exec! *src-port* "datahike" "INSERT INTO author VALUES (1, 'borges')")
  (exec! *src-port* "datahike" "INSERT INTO book VALUES (10, 1, 'Ficciones')")
  (exec! *src-port* "datahike" "INSERT INTO book VALUES (20, 1, 'Aleph')")
  (let [src (query-all *src-port* "datahike"
                       "SELECT id, author_id, title FROM book ORDER BY id")
        sql (dump/dump-to-string *src-conn*)
        tgt-rows (atom nil)]
    (with-target
      (fn [tport tdb]
        (replay-dump! sql tport tdb)
        (reset! tgt-rows (query-all tport tdb
                                    "SELECT id, author_id, title FROM book ORDER BY id"))))
    (is (= src @tgt-rows))))

;; ============================================================================
;; Mixed types + NULLs
;; ============================================================================

(deftest roundtrip-mixed-types-and-nulls
  (exec! *src-port* "datahike"
         (str "CREATE TABLE wide ("
              "id INTEGER PRIMARY KEY, "
              "name TEXT, "
              "price NUMERIC, "
              "active BOOLEAN, "
              "ratio DOUBLE PRECISION)"))
  (exec! *src-port* "datahike"
         "INSERT INTO wide VALUES (1, 'a', 9.99, true,  1.5)")
  (exec! *src-port* "datahike"
         "INSERT INTO wide VALUES (2, NULL, NULL, false, NULL)")
  (exec! *src-port* "datahike"
         "INSERT INTO wide VALUES (3, 'three', 0, true, 0.0)")
  (let [select "SELECT id, name, price, active, ratio FROM wide ORDER BY id"
        src    (query-all *src-port* "datahike" select)
        sql    (dump/dump-to-string *src-conn*)
        tgt    (atom nil)]
    (with-target
      (fn [tport tdb]
        (replay-dump! sql tport tdb)
        (reset! tgt (query-all tport tdb select))))
    (is (= src @tgt))))

;; ============================================================================
;; String escaping edge cases
;; ============================================================================

(deftest roundtrip-string-escaping-inserts
  ;; INSERT-format covers values that don't contain embedded
  ;; tabs/newlines/CRs. Those characters are stripped by the
  ;; server's pre-parse comment-stripper (a JSqlParser workaround)
  ;; on the way in via simple-query, so they need the COPY format
  ;; (raw bytes via CopyManager) instead — see the next test.
  (exec! *src-port* "datahike"
         "CREATE TABLE notes (id INTEGER PRIMARY KEY, body TEXT)")
  (exec! *src-port* "datahike"
         "INSERT INTO notes VALUES (1, 'with ''quote'' inside')")
  (with-open [c (DriverManager/getConnection (jdbc-url *src-port* "datahike"))
              ps (.prepareStatement c "INSERT INTO notes VALUES (?, ?)")]
    (.setInt ps 1 2) (.setString ps 2 "back\\slash and %percent") (.execute ps)
    (.setInt ps 1 3) (.setString ps 2 "trailing space ") (.execute ps))
  (let [select "SELECT id, body FROM notes ORDER BY id"
        src    (query-all *src-port* "datahike" select)
        sql    (dump/dump-to-string *src-conn* {:format :inserts})
        tgt    (atom nil)]
    (with-target
      (fn [tp tdb]
        (replay-dump! sql tp tdb)
        (reset! tgt (query-all tp tdb select))))
    (is (= src @tgt) "INSERT roundtrip preserves quotes/backslash/percent/trailing-space")))

(deftest roundtrip-string-escaping-copy
  ;; COPY-format is the supported path for tabs/newlines/CRs in
  ;; string fields — the wire-protocol COPY-IN sub-protocol delivers
  ;; raw bytes that bypass our SQL-side comment stripper.
  (exec! *src-port* "datahike"
         "CREATE TABLE notes (id INTEGER PRIMARY KEY, body TEXT)")
  (with-open [c (DriverManager/getConnection (jdbc-url *src-port* "datahike"))
              ps (.prepareStatement c "INSERT INTO notes VALUES (?, ?)")]
    ;; Note: tabs/newlines in source values come in via parameterised
    ;; Bind, which is also unaffected by stripComments — so the
    ;; source-side row already has the literal control chars.
    (.setInt ps 1 1) (.setString ps 2 "tab\there\nand newline") (.execute ps)
    (.setInt ps 1 2) (.setString ps 2 "carriage\rreturn") (.execute ps)
    (.setInt ps 1 3) (.setString ps 2 "with ''quote'' inside") (.execute ps))
  (let [select "SELECT id, body FROM notes ORDER BY id"
        src    (query-all *src-port* "datahike" select)
        sql    (dump/dump-to-string *src-conn* {:format :copy})
        tgt    (atom nil)]
    (with-target
      (fn [tp tdb]
        (replay-dump! sql tp tdb)
        (reset! tgt (query-all tp tdb select))))
    (is (= src @tgt) "COPY roundtrip preserves tabs/newlines/CR")))

;; ============================================================================
;; Sequences (DEFAULT nextval)
;; ============================================================================

(deftest roundtrip-sequence-and-setval
  ;; Tests that dump emits CREATE SEQUENCE + setval(...) so that the
  ;; sequence counter is preserved across a dump/replay. We use
  ;; explicit ids (not nextval-in-VALUES, which isn't currently
  ;; supported by our INSERT translator) and advance the source
  ;; sequence with an explicit setval to simulate prior nextval
  ;; activity.
  (exec! *src-port* "datahike" "CREATE SEQUENCE order_counter")
  (exec! *src-port* "datahike"
         "CREATE TABLE ord (id BIGINT PRIMARY KEY, label TEXT)")
  (exec! *src-port* "datahike" "INSERT INTO ord VALUES (1, 'first')")
  (exec! *src-port* "datahike" "INSERT INTO ord VALUES (2, 'second')")
  (exec! *src-port* "datahike" "INSERT INTO ord VALUES (3, 'third')")
  ;; Advance the sequence as if 3 nextvals had run. PG semantics:
  ;; with is_called=true, the NEXT nextval returns N+increment (i.e.
  ;; 4). We don't consume a nextval on the source — that would
  ;; advance internal state, and the dump emits the *current* value.
  (exec! *src-port* "datahike" "SELECT setval('order_counter', 3, true)")
  (let [select    "SELECT id, label FROM ord ORDER BY id"
        src       (query-all *src-port* "datahike" select)
        sql       (dump/dump-to-string *src-conn*)
        tgt-rows  (atom nil)
        tgt-next  (atom nil)]
    (with-target
      (fn [tport tdb]
        (replay-dump! sql tport tdb)
        (reset! tgt-rows (query-all tport tdb select))
        (reset! tgt-next (-> (query-all tport tdb
                                        "SELECT nextval('order_counter')")
                             first first))))
    (is (= src @tgt-rows) "rows match after dump/replay")
    ;; After replay, the first nextval should be 4 — the value any
    ;; client would have gotten after `setval(3, true)`. Proves the
    ;; sequence counter survived the dump/replay cycle.
    (is (= 4 @tgt-next)
        (str "post-replay nextval should be 4 (was: " @tgt-next ") — "
             "setval(3, true) was preserved across dump/replay"))))

;; ============================================================================
;; Empty database
;; ============================================================================

(deftest roundtrip-empty-database
  (let [sql (dump/dump-to-string *src-conn*)]
    (with-target
      (fn [tport tdb]
        (replay-dump! sql tport tdb)
        ;; If the dump had no DDL, the target should still be reachable
        (is (= [] (query-all tport tdb
                             "SELECT table_name FROM information_schema.tables WHERE table_schema='public'")))))))

;; ============================================================================
;; Many tables / many rows (sanity for emit ordering at scale)
;; ============================================================================

(deftest roundtrip-many-rows
  (exec! *src-port* "datahike"
         "CREATE TABLE bulk (id INTEGER PRIMARY KEY, payload TEXT)")
  (with-open [c (DriverManager/getConnection (jdbc-url *src-port* "datahike"))
              ps (.prepareStatement c "INSERT INTO bulk VALUES (?, ?)")]
    (doseq [i (range 250)]
      (.setInt ps 1 i)
      (.setString ps 2 (str "row-" i))
      (.execute ps)))
  (let [select "SELECT count(*) FROM bulk"
        src    (query-all *src-port* "datahike" select)
        sql    (dump/dump-to-string *src-conn*)
        tgt    (atom nil)]
    (with-target
      (fn [tport tdb]
        (replay-dump! sql tport tdb)
        (reset! tgt (query-all tport tdb select))))
    (is (= src @tgt))))
