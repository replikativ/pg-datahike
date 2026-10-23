(ns datahike.test.pg-cursor-protocol-test
  "DECLARE / FETCH / MOVE / CLOSE over the EXTENDED query protocol.

   Every existing cursor test drives the handler in process, which is
   the simple-query path. Over the extended protocol both of the
   statements that read a cursor were broken, and neither failure could
   be seen from the other side:

   `FETCH` reported NoData at Describe, so Execute then streamed
   DataRows the client had no field structure for -- pgjdbc raises
   `Received resultset tuples, but no field structure for them` and
   EVERY cursor read over JDBC failed. The identical bug, with the
   identical message, had already been found and fixed for `EXECUTE`.

   `MOVE ALL` called `.execute` on the handler from inside a statement,
   with the caller's `*cached-parsed*` still in scope, so it re-ran the
   OUTER plan -- itself -- until the stack ended and the connection
   died. The fourth caller to make that mistake.

   Expectations are a PostgreSQL 17 oracle's, driven through the same
   pgjdbc."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager ResultSet ResultSetMetaData
            SQLException Statement PreparedStatement]))

(def ^:dynamic *port* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0 :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"cur" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- ^Connection jdbc []
  (doto (DriverManager/getConnection
         (str "jdbc:postgresql://127.0.0.1:" *port*
              "/cur?user=x&password=x&sslmode=disable"))
    (.setAutoCommit false)))

(defn- update! [^Connection c sql]
  (with-open [s (.createStatement c)] (.executeUpdate s sql)))

(defn- rows [^Connection c sql]
  (with-open [s (.createStatement c) r (.executeQuery s sql)]
    (loop [acc []] (if (.next r) (recur (conj acc (.getInt r 1))) acc))))

(deftest fetch-describes-the-cursors-columns
  ;; Over the extended protocol this raised IllegalStateException on the
  ;; FIRST fetch, so cursors were unusable from any JDBC client.
  (with-open [c (jdbc)]
    (update! c "DECLARE cur CURSOR FOR SELECT g, g * 2 FROM generate_series(1,6) g")
    (with-open [s (.createStatement c) r (.executeQuery s "FETCH 2 FROM cur")]
      (let [^ResultSetMetaData m (.getMetaData r)]
        (testing "the RowDescription arrives, and it is the cursor's shape"
          (is (= 2 (.getColumnCount m)))
          (is (= "g" (.getColumnName m 1)))))
      (is (= [1 2] (loop [acc []] (if (.next r) (recur (conj acc (.getInt r 1))) acc)))))
    (testing "and through a PreparedStatement, which Describes separately"
      (with-open [p (.prepareStatement c "FETCH 1 FROM cur") r (.executeQuery p)]
        (is (= [3] (loop [acc []] (if (.next r) (recur (conj acc (.getInt r 1))) acc))))))))

(deftest move-all-does-not-recurse-into-its-own-plan
  ;; `MOVE ALL` ran its COUNT(*) through `.execute` on the handler while
  ;; the caller's `*cached-parsed*` was still bound, so the nested call
  ;; re-ran the outer MOVE. A StackOverflowError that took the
  ;; connection with it.
  (with-open [c (jdbc)]
    (update! c "DECLARE cur CURSOR FOR SELECT generate_series(1,5)")
    (with-open [p (.prepareStatement c "MOVE ALL IN cur")]
      (.execute p)
      (is (= 5 (.getUpdateCount p))))
    (testing "the cursor is exhausted, and the connection still works"
      (is (= [] (rows c "FETCH 1 FROM cur")))
      (update! c "CLOSE cur")
      (.commit c)
      (is (= [1] (rows c "SELECT 1"))))))

(deftest the-whole-cursor-surface-over-jdbc
  (with-open [c (jdbc)]
    (update! c "DECLARE cur CURSOR FOR SELECT g FROM generate_series(1,6) g")
    (is (= [1 2] (rows c "FETCH 2 FROM cur")))
    (with-open [p (.prepareStatement c "MOVE 1 IN cur")]
      (.execute p)
      (is (= 1 (.getUpdateCount p))))
    (is (= [4 5 6] (rows c "FETCH ALL FROM cur")))
    (with-open [p (.prepareStatement c "MOVE ALL IN cur")]
      (.execute p)
      (is (= 0 (.getUpdateCount p))))
    (update! c "CLOSE cur")
    (.commit c)
    (is (= [1] (rows c "SELECT 1")))))

(defn- pairs [^Connection c sql]
  (with-open [s (.createStatement c) r (.executeQuery s sql)]
    (loop [acc []]
      (if (.next r) (recur (conj acc [(.getInt r 1) (.getInt r 2)])) acc))))

(deftest cursor-paging-does-not-repeat-or-skip-rows
  ;; Each FETCH re-runs the stored query with `LIMIT n OFFSET pos`
  ;; (`rewrite-cursor-page`) rather than holding a position in one
  ;; result. Paging across RE-EXECUTIONS is only sound under a TOTAL
  ;; order, and the order injected for a cursor without ORDER BY is
  ;; `[1 :asc]`. If the first column has duplicates that order does not
  ;; decide which row wins a tie, so two executions are free to
  ;; disagree -- which would let one page repeat a row an earlier page
  ;; already returned, and skip another entirely.
  ;;
  ;; So the first column here is deliberately NON-unique (6 rows per
  ;; value) while the second identifies the row. The assertion is the
  ;; property a cursor must have whatever the paging strategy is:
  ;; every row exactly once.
  (with-open [c (jdbc)]
    (update! c "CREATE TABLE pag (id int, g int)")
    (update! c (str "INSERT INTO pag (id, g) VALUES "
                    (clojure.string/join
                     ", " (for [i (range 24)] (format "(%d, %d)" i (mod i 4))))))
    (doseq [page [1 3 5 7]]
      (testing (str "fetch_size " page)
        (update! c "BEGIN")
        (update! c "DECLARE pc CURSOR FOR SELECT g, id FROM pag")
        (let [seen (loop [acc []]
                     (let [batch (pairs c (str "FETCH " page " FROM pc"))]
                       (if (seq batch) (recur (into acc batch)) acc)))
              ids (mapv second seen)]
          (update! c "CLOSE pc")
          (update! c "COMMIT")
          (is (= 24 (count ids)) "no page may be short or over-long in total")
          (is (= (set (range 24)) (set ids)) "every row appears")
          (is (= (sort ids) (range 24))
              "and exactly once -- a repeat here means a page re-executed under a non-total order"))))))

(deftest fetch-and-move-agree-across-the-read-ahead-buffer
  ;; Each FETCH used to re-run the stored query with exactly the
  ;; client's page size, so reading N rows in pages of n cost N/n
  ;; executions whose OFFSETs sum to O(N^2/n). Measured on 20k rows at
  ;; postgres_fdw's page size of 100: 41.0s, and 139.8s with an ORDER
  ;; BY, against 287ms for the same rows in one shot.
  ;;
  ;; The underlying page now GROWS geometrically and the surplus is
  ;; buffered, which makes the total work O(N) while still letting a
  ;; client that peeks at a few rows pay for only a few. Same 20k rows
  ;; at page 100: 3.3s and 7.5s.
  ;;
  ;; The hazard that buys is `:pos`, which now counts what the
  ;; UNDERLYING query has yielded and so runs AHEAD of the client by
  ;; whatever sits in the buffer. MOVE reads it, so a MOVE has to drain
  ;; the buffer first and only then advance the offset -- otherwise it
  ;; skips the buffered rows twice. These page sizes are deliberately
  ;; not multiples of each other or of the doubling, so the reads land
  ;; inside a buffered chunk rather than on its boundary.
  (with-open [c (jdbc)]
    (update! c "CREATE TABLE mv (id int)")
    (update! c (str "INSERT INTO mv (id) VALUES "
                    (clojure.string/join ", " (for [i (range 40)] (format "(%d)" i)))))
    (testing "FETCH, MOVE, FETCH -- the MOVE must skip exactly what it says"
      (update! c "BEGIN")
      (update! c "DECLARE mc CURSOR FOR SELECT id FROM mv ORDER BY id")
      (is (= [0 1 2] (rows c "FETCH 3 FROM mc")))
      ;; 3 read, so the buffer is holding read-ahead here.
      (update! c "MOVE 5 FROM mc")
      (is (= [8 9] (rows c "FETCH 2 FROM mc"))
          "MOVE 5 skipped 3..7, not 3..7 twice")
      (update! c "MOVE 1 FROM mc")
      (is (= [11] (rows c "FETCH 1 FROM mc")))
      (update! c "CLOSE mc")
      (update! c "COMMIT"))
    (testing "FETCH ALL after a partial read returns exactly the remainder"
      (update! c "BEGIN")
      (update! c "DECLARE ac CURSOR FOR SELECT id FROM mv ORDER BY id")
      (is (= [0 1 2 3 4 5 6] (rows c "FETCH 7 FROM ac")))
      (let [rest-rows (rows c "FETCH ALL FROM ac")]
        (is (= 33 (count rest-rows)) "33 of 40 left")
        (is (= (vec (range 7 40)) rest-rows) "in order, none repeated"))
      (is (= [] (rows c "FETCH 1 FROM ac")) "and the cursor is spent")
      (update! c "CLOSE ac")
      (update! c "COMMIT"))
    (testing "a page larger than the whole relation is not short-changed"
      (update! c "BEGIN")
      (update! c "DECLARE bc CURSOR FOR SELECT id FROM mv ORDER BY id")
      (is (= 40 (count (rows c "FETCH 500 FROM bc"))))
      (is (= [] (rows c "FETCH 500 FROM bc")))
      (update! c "CLOSE bc")
      (update! c "COMMIT"))))

(deftest a-cursors-position-is-postgresqls
  ;; Every answer below was read off PostgreSQL 17 for the same
  ;; statements. Before this, only NEXT/FORWARD n and ALL were served:
  ;; `FETCH FORWARD ALL` returned ONE row (the parser took FORWARD and
  ;; never saw ALL -- what psycopg2's named-cursor fetchall() sends),
  ;; `FETCH ABSOLUTE 4` returned rows 1..4, `FETCH LAST` the next row,
  ;; `FETCH 0` nothing, `MOVE 10` past the end claimed `MOVE 10`.
  (with-open [c (jdbc)]
    (update! c "CREATE TABLE ct (g int)")
    (update! c "INSERT INTO ct (g) VALUES (1),(2),(3),(4),(5),(6),(7),(8)")
    (testing "a NO SCROLL cursor moves forward by every spelling"
      (update! c "BEGIN")
      (update! c "DECLARE c CURSOR FOR SELECT g FROM ct ORDER BY g")
      (is (= [1 2] (rows c "FETCH FORWARD 2 FROM c")))
      (is (= [2] (rows c "FETCH 0 FROM c")) "FETCH 0 is the current row")
      (is (= [4] (rows c "FETCH RELATIVE 2 FROM c")))
      (is (= [6] (rows c "FETCH ABSOLUTE 6 FROM c")))
      (is (= 2 (update! c "MOVE 10 IN c")) "the tag counts the rows passed")
      (is (= [] (rows c "FETCH 0 FROM c")) "past the end, on no row")
      (update! c "COMMIT"))
    (testing "FORWARD ALL is ALL, and LAST is the last row"
      (update! c "BEGIN")
      (update! c "DECLARE c CURSOR FOR SELECT g FROM ct ORDER BY g")
      (is (= [1 2 3 4 5 6 7 8] (rows c "FETCH FORWARD ALL FROM c")))
      (update! c "CLOSE c")
      (update! c "DECLARE c CURSOR FOR SELECT g FROM ct ORDER BY g")
      (is (= [7] (rows c "FETCH ABSOLUTE -2 FROM c")))
      (is (= [8] (rows c "FETCH LAST FROM c")))
      (update! c "COMMIT"))
    (testing "going back on a NO SCROLL cursor is PostgreSQL's 55000"
      (update! c "BEGIN")
      (update! c "DECLARE c NO SCROLL CURSOR FOR SELECT g FROM ct ORDER BY g")
      (is (= [1 2] (rows c "FETCH 2 FROM c")))
      (let [e (try (rows c "FETCH PRIOR FROM c") nil
                   (catch SQLException e e))]
        (is (= "55000" (some-> e .getSQLState))))
      (update! c "ROLLBACK"))
    (testing "a SCROLL cursor goes back over the rows it has read"
      (update! c "BEGIN")
      (update! c "DECLARE c SCROLL CURSOR FOR SELECT g FROM ct ORDER BY g")
      (is (= [1 2 3 4 5 6 7 8] (rows c "FETCH ALL FROM c")))
      (is (= [8 7 6] (rows c "FETCH BACKWARD 3 FROM c")))
      (is (= [5] (rows c "FETCH PRIOR FROM c")))
      (is (= [3] (rows c "FETCH ABSOLUTE 3 FROM c")))
      (is (= [2 1] (rows c "FETCH BACKWARD ALL FROM c")))
      (is (= [1] (rows c "FETCH NEXT FROM c")))
      (is (= [8] (rows c "FETCH LAST FROM c")))
      (is (= 2 (update! c "MOVE -2 IN c")))
      (is (= [7] (rows c "FETCH 1 FROM c")))
      (is (= ["t"] (with-open [s (.createStatement c)
                               r (.executeQuery s "SELECT is_scrollable FROM pg_cursors")]
                     (loop [acc []] (if (.next r) (recur (conj acc (.getString r 1))) acc)))))
      (update! c "COMMIT"))
    (testing "a row cap the page rewrite cannot read still caps the cursor"
      ;; FETCH FIRST is not a LIMIT node, so folding the page into it
      ;; replaced the cap: FETCH ALL answered 6 of these rows, not 3.
      (update! c "BEGIN")
      (update! c "DECLARE c CURSOR FOR SELECT g FROM ct ORDER BY g FETCH FIRST 3 ROWS ONLY")
      (is (= [1 2 3] (rows c "FETCH ALL FROM c")))
      (update! c "CLOSE c")
      (update! c "DECLARE c CURSOR FOR SELECT g FROM ct ORDER BY g LIMIT 5 OFFSET 2")
      (is (= [3 4] (rows c "FETCH 2 FROM c")))
      (is (= [5 6 7] (rows c "FETCH ALL FROM c")))
      (update! c "COMMIT"))))
