(ns datahike.test.pg-declared-param-oid-test
  "Issue #27 — a parameter's type OID as *declared by the client in the
   Parse message* must survive to both ParameterDescription and, for a
   bare `$N` output column, RowDescription.

   PostgreSQL resolves parameter types at parse-analyze: a non-zero OID
   in Parse is authoritative and only the 0 (\"unknown\") slots get
   inferred from context. We used to answer Describe('S', …) by
   re-running our own inference and discarding the client's declaration
   entirely, so:

     - `SELECT $1` with a declared int2 described as text(25) — the
       reported symptom of issue #27, also seen for int4/int8/float4.
     - pgjdbc's `executeBatch` on a parameterized INSERT failed
       client-side with `Can't change resolved type for param: 2 from
       1043 to 25`: it resolved `setString` to varchar(1043), we
       answered text(25). (Reported as follow-up 3 on PR #30.)

   These tests speak the wire protocol directly rather than going
   through pgjdbc, because pgjdbc chooses the declared OIDs itself —
   only a raw Parse can pin them to a specific value, and only reading
   the raw ParameterDescription/RowDescription can check the answer.

   Verified against PostgreSQL 17 by differential testing; the expected
   OIDs and typlens here are what a real server returns."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer PgWireServer$QueryHandler
            PgWireServer$QueryHandlerFactory PgWireServer$QueryResult]
           [java.io DataInputStream DataOutputStream]
           [java.net Socket]
           [java.nio.charset StandardCharsets]))

(def oid-bool 16)
(def oid-int8 20)
(def oid-int2 21)
(def oid-int4 23)
(def oid-text 25)
(def oid-float4 700)
(def oid-float8 701)
(def oid-varchar 1043)
(def oid-date 1082)
(def oid-numeric 1700)
(def oid-int4-array 1007)

(def ^:dynamic *port* nil)
(def ^:dynamic *conn* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          factory (reify PgWireServer$QueryHandlerFactory
                    (create [_] (pg/make-query-handler conn)))
          server (PgWireServer. 0 "127.0.0.1" factory)]
      (.start server)
      (try
        (binding [*port* (.getPort server) *conn* conn]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

;; ---------------------------------------------------------------------------
;; Minimal raw pgwire client — enough for startup + Parse/Describe/Bind/
;; Execute/Sync, decoding the messages these tests assert on.
;; ---------------------------------------------------------------------------

(defn- cstr ^bytes [^String s]
  (let [b (.getBytes s StandardCharsets/UTF_8)
        out (byte-array (inc (alength b)))]
    (System/arraycopy b 0 out 0 (alength b))
    out))

(defn- send-msg! [^DataOutputStream out tag ^bytes body]
  (when tag (.writeByte out (int tag)))
  (.writeInt out (+ 4 (alength body)))
  (.write out body)
  (.flush out))

(defn- ba
  "Concatenate byte arrays / ints-as-int32 / shorts-as-int16 into one array."
  ^bytes [& parts]
  (let [bos (java.io.ByteArrayOutputStream.)
        dos (DataOutputStream. bos)]
    (doseq [p parts]
      (cond
        (bytes? p)             (.write dos ^bytes p)
        (and (vector? p) (= :i32 (first p))) (.writeInt dos (int (second p)))
        (and (vector? p) (= :i16 (first p))) (.writeShort dos (int (second p)))
        :else (throw (ex-info "bad part" {:p p}))))
    (.flush dos)
    (.toByteArray bos)))

(defn- read-msg
  "Read one backend message. Returns [tag-char ^bytes body]."
  [^DataInputStream in]
  (let [tag (char (.readByte in))
        len (.readInt in)
        body (byte-array (- len 4))]
    (.readFully in body)
    [tag body]))

(defn- read-until-ready [^DataInputStream in]
  (loop [acc []]
    (let [[tag body] (read-msg in)
          acc (conj acc [tag body])]
      (if (= \Z tag) acc (recur acc)))))

(defn- buf ^java.nio.ByteBuffer [^bytes b] (java.nio.ByteBuffer/wrap b))

(defn- read-cstring [^java.nio.ByteBuffer bb]
  (let [sb (StringBuilder.)]
    (loop []
      (let [c (.get bb)]
        (when-not (zero? c)
          (.append sb (char c))
          (recur))))
    (.toString sb)))

(defn- decode
  "Turn the raw message list into the shapes these tests assert on."
  [msgs]
  (reduce
   (fn [acc [tag ^bytes body]]
     (case tag
       ;; ParameterDescription
       \t (let [bb (buf body)
                n (.getShort bb)]
            (assoc acc :param-oids (vec (repeatedly n #(.getInt bb)))))
       ;; RowDescription — one entry per column
       \T (let [bb (buf body)
                n (.getShort bb)]
            (assoc acc :columns
                   (vec (repeatedly
                         n (fn []
                             (let [nm (read-cstring bb)
                                   _tbl (.getInt bb)
                                   _col (.getShort bb)
                                   oid (.getInt bb)
                                   typlen (.getShort bb)
                                   typmod (.getInt bb)
                                   fmt (.getShort bb)]
                               {:name nm :oid oid :typlen typlen
                                :typmod typmod :format fmt}))))))
       ;; DataRow
       \D (let [bb (buf body)
                n (.getShort bb)
                fields (vec (repeatedly
                             n (fn []
                                 (let [len (.getInt bb)]
                                   (when-not (neg? len)
                                     (let [b (byte-array len)]
                                       (.get bb b)
                                       b))))))
                row (mapv #(when % (String. ^bytes % StandardCharsets/UTF_8)) fields)
                raw-row (mapv #(when % (mapv (fn [b] (bit-and (int b) 0xff)) %))
                              fields)]
            (-> acc
                (update :rows (fnil conj []) row)
                (update :raw-rows (fnil conj []) raw-row)))
       \n (assoc acc :no-data? true)
       \C (assoc acc :command-complete
                 (String. body 0 (dec (alength body)) StandardCharsets/UTF_8))
       \s (assoc acc :portal-suspended? true)
       \E (let [bb (buf body)]
            (assoc acc :error
                   (loop [m {}]
                     (let [c (.get bb)]
                       (if (zero? c)
                         m
                         (recur (assoc m (char c) (read-cstring bb))))))))
       acc))
   {}
   msgs))

(defn- with-conn
  "Open a startup-completed pgwire connection and call (f in out)."
  [f]
  (with-open [sock (Socket. "127.0.0.1" (int *port*))]
    (.setSoTimeout sock 15000)
    (let [out (DataOutputStream. (.getOutputStream sock))
          in (DataInputStream. (.getInputStream sock))]
      (send-msg! out nil (ba [:i32 196608]
                             (cstr "user") (cstr "x")
                             (cstr "database") (cstr "test")
                             (byte-array 1)))
      ;; Drain startup: auth request (answer with an empty password if
      ;; asked) through ReadyForQuery.
      (loop []
        (let [[tag ^bytes body] (read-msg in)]
          (case tag
            \R (do (when-not (zero? (.getInt (buf body)))
                     (send-msg! out \p (cstr "")))
                   (recur))
            \Z nil
            \E (throw (ex-info "startup failed" {:body (String. body)}))
            (recur))))
      (f in out))))

(defn- bind-body-formats
  ([portal-name statement-name params result-formats]
   (bind-body-formats portal-name statement-name params result-formats false))
  ([portal-name statement-name params result-formats binary-params?]
   (apply ba (cstr portal-name) (cstr statement-name)
          (concat (if binary-params? [[:i16 1] [:i16 1]] [[:i16 0]])
                  [[:i16 (count params)]]
                  (mapcat (fn [p]
                            (if (nil? p)
                              [[:i32 -1]]
                              (let [^bytes b (if binary-params?
                                               p
                                               (.getBytes ^String p StandardCharsets/UTF_8))]
                                [[:i32 (alength b)] b])))
                          params)
                  [[:i16 (count result-formats)]]
                  (map (fn [format] [:i16 format]) result-formats)))))

(defn- bind-body [statement-name params]
  (bind-body-formats "" statement-name params []))

(defn- send-bind-execute-sync! [^DataOutputStream out statement-name params]
  (send-msg! out \B (bind-body statement-name params))
  (send-msg! out \E (ba (cstr "") [:i32 0]))
  (send-msg! out \S (byte-array 0)))

(defn- read-until-tags [^DataInputStream in terminals]
  (loop [acc []]
    (let [[tag body :as msg] (read-msg in)
          acc (conj acc msg)]
      (if (terminals tag) acc (recur acc)))))

(defn- read-until-tag [in terminal]
  (read-until-tags in #{terminal}))

(defn- parse-describe-execute-many
  "Parse and Describe one named statement, then Bind and Execute it for
   every parameter vector. Returns one decoded protocol cycle per Bind.

   Reusing the parsed statement is important: it verifies that lowering
   did not capture values from the first Bind and that NULL in a later
   Bind does not change its already-resolved parameter or result types."
  [sql param-oids param-sets]
  (with-conn
    (fn [in out]
      (let [statement-name "matrix"]
        (send-msg! out \P (apply ba (cstr statement-name) (cstr sql)
                                 [:i16 (count param-oids)]
                                 (map (fn [o] [:i32 o]) param-oids)))
        (send-msg! out \D (ba (.getBytes "S" StandardCharsets/UTF_8)
                              (cstr statement-name)))
        (mapv (fn [params]
                (send-bind-execute-sync! out statement-name params)
                (decode (read-until-ready in)))
              param-sets)))))

(defn- parse-describe-execute
  "Parse `sql` declaring `param-oids`, Describe the statement, Bind
   `params` (text format) and Execute. Returns the decoded messages."
  [sql param-oids params]
  (first (parse-describe-execute-many sql param-oids [params])))

(defn- simple [sql]
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr sql))
      (decode (read-until-ready in)))))

(deftest select-without-projection-returns-one-row
  ;; #281. A zero-column row still has RowDescription and DataRow messages.
  (doseq [result [(simple "SELECT /* no projection */;")
                  (parse-describe-execute "SELECT" [] [])]]
    (is (nil? (:error result)) (:error result))
    (is (= [] (:columns result)))
    (is (= [[]] (:rows result)))
    (is (= "SELECT 1" (:command-complete result))))
  (is (= [[";"]] (:rows (simple "SELECT ';';")))))

(deftest reported-literal-and-range-regressions
  ;; #374, #286 and #287, checked against a PostgreSQL 18.6 UTF-8 database.
  (is (= [["1" "2"]] (:rows (simple "SELECT length('🥝'), char_length('é')"))))
  (let [result (simple "SELECT ARRAY[1,NULL,55]")]
    (is (= [oid-int4-array] (mapv :oid (:columns result))))
    (is (= [["{1,NULL,55}"]] (:rows result))))
  (is (= "22003" (get-in (simple "SELECT exp(20000.0)") [:error \C]))))

(deftest now-as-time-reads-the-current-clock
  ;; #282. The literal and CURRENT_TIME agree when parsed and run together.
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr "BEGIN; SELECT 'now'::time = CURRENT_TIME::time;"))
      (is (= [["t"]] (:rows (decode (read-until-ready in)))))))
  (let [result (parse-describe-execute "SELECT 'now'::time" [] [])]
    (is (= [1083] (mapv :oid (:columns result))))
    (is (re-matches #"\d{2}:\d{2}:\d{2}(?:\.\d+)?" (or (ffirst (:rows result)) ""))))
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr "PREPARE capture_time AS SELECT 'now'::time = CURRENT_TIME::time"))
      (is (nil? (:error (decode (read-until-ready in)))))
      (Thread/sleep 20)
      (send-msg! out \Q (cstr "EXECUTE capture_time"))
      (is (= [["f"]] (:rows (decode (read-until-ready in))))))))

(deftest varbit-parameters-use-the-bit-operator
  ;; #283. Both operands are declared varbit. PostgreSQL describes the result as bit.
  (is (= [["1000"]] (:rows (parse-describe-execute "SELECT $1 & $2" [1562 1562] ["1010" "1100"]))))
  (with-conn
    (fn [in out]
      (send-msg! out \P (ba (cstr "bits") (cstr "SELECT $1 & $2") [:i16 2] [:i32 1562] [:i32 1562]))
      (send-msg! out \B (bind-body-formats "" "bits"
                                           [(ba [:i32 4] (byte-array [(unchecked-byte 0xa0)]))
                                            (ba [:i32 4] (byte-array [(unchecked-byte 0xc0)]))]
                                           [1] true))
      (send-msg! out \D (ba (byte-array [(int \P)]) (cstr "")))
      (send-msg! out \E (ba (cstr "") [:i32 0]))
      (send-msg! out \S (byte-array 0))
      (let [result (decode (read-until-ready in))]
        (is (= [1560] (mapv :oid (:columns result))))
        (is (= [[[0 0 0 4 128]]] (:raw-rows result)))))))

(deftest insert-from-unnest-of-a-binary-array
  ;; #285. Parse has the type but the array contents arrive only at Bind.
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr "CREATE TABLE sarray(id serial PRIMARY KEY, val int2)"))
      (is (nil? (:error (decode (read-until-ready in)))))
      (send-msg! out \P (ba (cstr "load") (cstr "INSERT INTO sarray(val) SELECT * FROM unnest($1::int2[])")
                            [:i16 1] [:i32 1005]))
      (send-msg! out \B (bind-body-formats "" "load"
                                           [(ba [:i32 1] [:i32 0] [:i32 21] [:i32 2] [:i32 1]
                                                [:i32 2] [:i16 2] [:i32 2] [:i16 -3])]
                                           [] true))
      (send-msg! out \E (ba (cstr "") [:i32 0]))
      (send-msg! out \S (byte-array 0))
      (is (= "INSERT 0 2" (:command-complete (decode (read-until-ready in)))))
      (send-msg! out \Q (cstr "SELECT val FROM sarray ORDER BY id"))
      (let [result (decode (read-until-ready in))]
        (is (= [oid-int2] (mapv :oid (:columns result))))
        (is (= [["2"] ["-3"]] (:rows result)))))))

(deftest execute-max-rows-suspends-and-resumes-one-portal
  (with-conn
    (fn [in out]
      (let [statement-name "paged"
            sql "SELECT * FROM generate_series(1,5) AS g"]
        (send-msg! out \P (ba (cstr statement-name) (cstr sql) [:i16 0]))
        (send-msg! out \B (bind-body statement-name []))

        ;; Execute and Flush twice without rebinding. The second page must
        ;; continue at row 3; rerunning the handler would repeat rows 1 and 2.
        (send-msg! out \E (ba (cstr "") [:i32 2]))
        (send-msg! out \H (byte-array 0))
        (let [page-1 (decode (read-until-tag in \s))]
          (is (= [["1"] ["2"]] (:rows page-1)))
          (is (:portal-suspended? page-1))
          (is (nil? (:command-complete page-1))))

        (send-msg! out \E (ba (cstr "") [:i32 2]))
        (send-msg! out \H (byte-array 0))
        (let [page-2 (decode (read-until-tag in \s))]
          (is (= [["3"] ["4"]] (:rows page-2)))
          (is (:portal-suspended? page-2))
          ;; RowDescription is a one-time portal response, not repeated on
          ;; every resumed Execute.
          (is (nil? (:columns page-2))))

        (send-msg! out \E (ba (cstr "") [:i32 2]))
        (send-msg! out \H (byte-array 0))
        (let [page-3 (decode (read-until-tag in \C))]
          (is (= [["5"]] (:rows page-3)))
          ;; CommandComplete counts rows processed by this Execute, not the
          ;; statement's total cardinality.
          (is (= "SELECT 1" (:command-complete page-3)))
          (is (not (:portal-suspended? page-3))))

        ;; Before transaction end, Execute at EOF reports zero rows and does
        ;; not rerun or retain/replay the completed row array.
        (send-msg! out \E (ba (cstr "") [:i32 2]))
        (send-msg! out \H (byte-array 0))
        (let [exhausted (decode (read-until-tag in \C))]
          (is (nil? (:rows exhausted)))
          (is (= "SELECT 0" (:command-complete exhausted))))

        ;; Sync ends the implicit transaction and drops non-holdable portals.
        (send-msg! out \S (byte-array 0))
        (read-until-ready in)
        (send-msg! out \E (ba (cstr "") [:i32 2]))
        (send-msg! out \S (byte-array 0))
        (let [after-sync (decode (read-until-ready in))]
          (is (= "34000" (get-in after-sync [:error \C]))))))))

(deftest exact-page-boundary-suspends-before-eof-probe
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr "BEGIN"))
      (read-until-ready in)
      (send-msg! out \P (ba (cstr "exact_stmt")
                            (cstr "SELECT * FROM generate_series(1,2)")
                            [:i16 0]))
      (send-msg! out \B (bind-body-formats "exact_portal" "exact_stmt" [] []))
      (send-msg! out \E (ba (cstr "exact_portal") [:i32 2]))
      (send-msg! out \H (byte-array 0))
      (let [page (decode (read-until-tag in \s))]
        (is (= [["1"] ["2"]] (:rows page)))
        (is (:portal-suspended? page))
        (is (nil? (:command-complete page))))

      ;; PostgreSQL only discovers exhaustion on the next Execute because
      ;; the producer stopped exactly at maxRows.
      (send-msg! out \E (ba (cstr "exact_portal") [:i32 2]))
      (send-msg! out \S (byte-array 0))
      (let [eof (decode (read-until-ready in))]
        (is (nil? (:rows eof)))
        (is (= "SELECT 0" (:command-complete eof)))))))

(deftest portal-describe-advertises-bound-result-format
  (with-conn
    (fn [in out]
      (let [statement-name "fmt"
            portal-name "binary"]
        (send-msg! out \P (ba (cstr statement-name) (cstr "SELECT 42") [:i16 0]))
        (send-msg! out \B (bind-body-formats portal-name statement-name [] [1]))
        (send-msg! out \D (ba (.getBytes "P" StandardCharsets/UTF_8)
                              (cstr portal-name)))
        (send-msg! out \S (byte-array 0))
        (let [result (decode (read-until-ready in))]
          (is (= 1 (:format (first (:columns result))))))))))

(deftest suspended-portal-survives-sync-only-inside-explicit-transaction
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr "BEGIN"))
      (read-until-ready in)
      (send-msg! out \P (ba (cstr "tx_stmt")
                            (cstr "SELECT * FROM generate_series(1,2)")
                            [:i16 0]))
      (send-msg! out \B (bind-body-formats "tx_portal" "tx_stmt" [] []))
      (send-msg! out \E (ba (cstr "tx_portal") [:i32 1]))
      (send-msg! out \S (byte-array 0))
      (let [first-page (decode (read-until-ready in))]
        (is (= [["1"]] (:rows first-page)))
        (is (:portal-suspended? first-page)))

      ;; Sync does not end an explicit transaction, so the named portal can
      ;; resume. The final tag counts only this Execute's row.
      (send-msg! out \E (ba (cstr "tx_portal") [:i32 1]))
      (send-msg! out \S (byte-array 0))
      (let [second-page (decode (read-until-ready in))]
        (is (= [["2"]] (:rows second-page)))
        (is (:portal-suspended? second-page)))

      (send-msg! out \E (ba (cstr "tx_portal") [:i32 1]))
      (send-msg! out \S (byte-array 0))
      (let [eof (decode (read-until-ready in))]
        (is (= "SELECT 0" (:command-complete eof))))

      ;; COMMIT is a Simple Query transaction boundary and must invalidate
      ;; the non-holdable extended-query portal as well.
      (send-msg! out \Q (cstr "COMMIT"))
      (read-until-ready in)
      (send-msg! out \E (ba (cstr "tx_portal") [:i32 1]))
      (send-msg! out \S (byte-array 0))
      (is (= "34000"
             (get-in (decode (read-until-ready in)) [:error \C]))))))

(deftest named-portal-requires-close-before-rebind
  (with-conn
    (fn [in out]
      (send-msg! out \P (ba (cstr "named_stmt") (cstr "SELECT 1") [:i16 0]))
      (send-msg! out \B (bind-body-formats "named_portal" "named_stmt" [] []))
      (send-msg! out \B (bind-body-formats "named_portal" "named_stmt" [] []))
      (send-msg! out \S (byte-array 0))
      (is (= "42P03"
             (get-in (decode (read-until-ready in)) [:error \C]))))))

(deftest extended-commit-drops-portals-before-sync
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr "BEGIN"))
      (read-until-ready in)
      (send-msg! out \P (ba (cstr "old_stmt")
                            (cstr "SELECT * FROM generate_series(1,2)")
                            [:i16 0]))
      (send-msg! out \B (bind-body-formats "old_portal" "old_stmt" [] []))
      (send-msg! out \E (ba (cstr "old_portal") [:i32 1]))
      (send-msg! out \H (byte-array 0))
      (is (:portal-suspended? (decode (read-until-tag in \s))))

      ;; Pipeline an old-portal Execute after COMMIT but before Sync. The
      ;; transaction command destroys that portal during its own Execute.
      (send-msg! out \P (ba (cstr "commit_stmt") (cstr "COMMIT") [:i16 0]))
      (send-msg! out \B (bind-body-formats "commit_portal" "commit_stmt" [] []))
      (send-msg! out \E (ba (cstr "commit_portal") [:i32 0]))
      (send-msg! out \E (ba (cstr "old_portal") [:i32 1]))
      (send-msg! out \S (byte-array 0))
      (let [result (decode (read-until-ready in))]
        (is (= "COMMIT" (:command-complete result)))
        (is (= "34000" (get-in result [:error \C])))))))

(deftest nonempty-simple-query-replaces-unnamed-portal-inside-transaction
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr "BEGIN"))
      (read-until-ready in)
      (send-msg! out \P (ba (cstr "")
                            (cstr "SELECT * FROM generate_series(1,2)")
                            [:i16 0]))
      (send-msg! out \B (bind-body "" []))
      (send-msg! out \E (ba (cstr "") [:i32 1]))
      (send-msg! out \H (byte-array 0))
      (is (:portal-suspended? (decode (read-until-tag in \s))))

      (send-msg! out \Q (cstr "SELECT 99"))
      (is (= [["99"]] (:rows (decode (read-until-ready in)))))
      (send-msg! out \E (ba (cstr "") [:i32 1]))
      (send-msg! out \S (byte-array 0))
      (is (= "34000"
             (get-in (decode (read-until-ready in)) [:error \C]))))))

(deftest binary-data-rows-retain-format-across-pages
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr "BEGIN"))
      (read-until-ready in)
      (send-msg! out \P (ba (cstr "binary_stmt")
                            (cstr "SELECT x::int4 FROM generate_series(1,3) AS x")
                            [:i16 0]))
      (send-msg! out \B (bind-body-formats "binary_portal" "binary_stmt" [] [1]))
      (send-msg! out \E (ba (cstr "binary_portal") [:i32 2]))
      (send-msg! out \H (byte-array 0))
      (let [first-page (decode (read-until-tag in \s))]
        (is (= [[[0 0 0 1]] [[0 0 0 2]]] (:raw-rows first-page))))
      (send-msg! out \E (ba (cstr "binary_portal") [:i32 2]))
      (send-msg! out \S (byte-array 0))
      (let [second-page (decode (read-until-ready in))]
        (is (= [[[0 0 0 3]]] (:raw-rows second-page)))
        (is (= "SELECT 1" (:command-complete second-page)))))))

(deftest returning-command-tags-count-each-execute-page
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr "CREATE TABLE portal_returning (id int PRIMARY KEY)"))
      (read-until-ready in)
      (send-msg! out \Q (cstr "BEGIN"))
      (read-until-ready in)
      (send-msg! out \P
                 (ba (cstr "returning_stmt")
                     (cstr (str "INSERT INTO portal_returning VALUES (1), (2), (3) "
                                "RETURNING id"))
                     [:i16 0]))
      (send-msg! out \B
                 (bind-body-formats "returning_portal" "returning_stmt" [] []))
      (send-msg! out \E (ba (cstr "returning_portal") [:i32 2]))
      (send-msg! out \H (byte-array 0))
      (let [first-page (decode (read-until-tag in \s))]
        (is (= 2 (count (:rows first-page))))
        (is (nil? (:command-complete first-page))))

      (send-msg! out \E (ba (cstr "returning_portal") [:i32 2]))
      (send-msg! out \H (byte-array 0))
      (let [final-page (decode (read-until-tag in \C))]
        (is (= 1 (count (:rows final-page))))
        (is (= "INSERT 0 1" (:command-complete final-page))))

      (send-msg! out \E (ba (cstr "returning_portal") [:i32 2]))
      (send-msg! out \S (byte-array 0))
      (let [eof (decode (read-until-ready in))]
        (is (= "INSERT 0 0" (:command-complete eof)))))))

(deftest nonpositive-execute-row-limit-means-fetch-all
  ;; PostgreSQL's exec_execute_message treats every max_rows <= 0 as
  ;; FETCH_ALL. Exercise -1 explicitly; zero is used throughout the helpers.
  (with-conn
    (fn [in out]
      (let [statement-name "all"]
        (send-msg! out \P
                   (ba (cstr statement-name)
                       (cstr "SELECT * FROM generate_series(1,3) AS g")
                       [:i16 0]))
        (send-msg! out \B (bind-body statement-name []))
        (send-msg! out \E (ba (cstr "") [:i32 -1]))
        (send-msg! out \S (byte-array 0))
        (let [result (decode (read-until-ready in))]
          (is (= [["1"] ["2"] ["3"]] (:rows result)))
          (is (= "SELECT 3" (:command-complete result)))
          (is (not (:portal-suspended? result))))))))
(deftest portal-pins-plan-and-description-at-bind
  (let [token (atom 0)
        width (atom 1)
        handler (reify PgWireServer$QueryHandler
                  (execute [_ _] (PgWireServer$QueryResult/empty "SELECT 0"))
                  (parse [_ _ _] @width)
                  (describeParams [_ _] (int-array 0))
                  (describeResult [_ parsed]
                    (PgWireServer$QueryResult.
                     (into-array String (take parsed ["a" "b"]))
                     (int-array (repeat parsed oid-text))
                     (into-array (Class/forName "[Ljava.lang.String;")
                                 (make-array String 0 0))
                     "SELECT 0"))
                  (executePrepared [_ parsed _]
                    (PgWireServer$QueryResult.
                     (into-array String (take parsed ["a" "b"]))
                     (int-array (repeat parsed oid-text))
                     (into-array (Class/forName "[Ljava.lang.String;")
                                 [(into-array String (take parsed ["old" "new"]))])
                     "SELECT 1"))
                  (planCacheToken [_] @token))
        factory (reify PgWireServer$QueryHandlerFactory
                  (create [_] handler))
        server (PgWireServer. 0 "127.0.0.1" factory)]
    (.start server)
    (try
      (binding [*port* (.getPort server)]
        (with-conn
          (fn [in out]
            (send-msg! out \P (ba (cstr "s") (cstr "SELECT ignored") [:i16 0]))
            (send-msg! out \B (ba (cstr "p") (cstr "s")
                                  [:i16 0] [:i16 0] [:i16 0]))
            ;; Flush observes Parse/Bind completion without Sync ending the
            ;; implicit transaction and destroying its non-holdable portal.
            (send-msg! out \H (byte-array 0))
            (read-until-tag in \2)
            ;; Invalidate the statement after Bind. A Portal owns the old
            ;; plan and descriptor, just as PostgreSQL's Portal does.
            (reset! width 2)
            (swap! token inc)
            (send-msg! out \D (ba (.getBytes "P" StandardCharsets/UTF_8)
                                  (cstr "p")))
            (send-msg! out \E (ba (cstr "p") [:i32 0]))
            (send-msg! out \S (byte-array 0))
            (let [result (decode (read-until-ready in))]
              (is (= ["a"] (mapv :name (:columns result))))
              (is (= [["old"]] (:rows result)))))))
      (finally
        (.stop server)))))
;; ---------------------------------------------------------------------------
;; ParameterDescription echoes the client's declaration
;; ---------------------------------------------------------------------------

(deftest declared-param-oid-echoed-in-parameter-description
  (testing "a non-zero declared OID is returned verbatim, not re-inferred"
    (doseq [oid [oid-int2 oid-int4 oid-int8 oid-float4 oid-float8
                 oid-bool oid-numeric oid-varchar oid-date oid-text]]
      (is (= [oid] (:param-oids (parse-describe-execute "SELECT $1" [oid]
                                                        [(if (= oid oid-date) "2024-01-15" "1")])))
          (str "declared OID " oid " should survive Describe")))))

(deftest declared-routine-oids-read-text-and-preserve-identity
  (doseq [[oid input want] [[24 "1299" "now"] [24 "-" "-"]
                            [2202 "abs(int4)" "abs(integer)"]]]
    (let [result (parse-describe-execute "SELECT $1" [oid] [input])]
      (is (nil? (:error result)))
      (is (= [oid] (:param-oids result)))
      (is (= 4 (:typlen (first (:columns result)))))
      (is (= [[want]] (:rows result)))))
  (let [result (parse-describe-execute "SELECT $1" [24] ["abs"])]
    (is (= "42725" (get-in result [:error \C])))))

(deftest routine-array-parameters-are-read-even-when-unused
  (let [result (parse-describe-execute "SELECT $1" [1008] ["{1299,0}"])]
    (is (nil? (:error result)))
    (is (= [["{now,-}"]] (:rows result))))
  (is (= "42725" (get-in (parse-describe-execute "SELECT 1" [1008] ["{abs}"])
                         [:error \C]))))

(deftest oid-alias-binary-parameters-preserve-unsigned-values
  (doseq [oid [24 2202 2205 2206 4089]]
    (with-conn
      (fn [in out]
        (send-msg! out \P (ba (cstr "routine_binary") (cstr "SELECT $1")
                              [:i16 1] [:i32 oid]))
        (send-msg! out \D (ba (.getBytes "S" StandardCharsets/UTF_8) (cstr "routine_binary")))
        (send-msg! out \B (ba (cstr "") (cstr "routine_binary")
                              [:i16 1] [:i16 1] [:i16 1] [:i32 4] [:i32 -1] [:i16 0]))
        (send-msg! out \E (ba (cstr "") [:i32 0]))
        (send-msg! out \S (byte-array 0))
        (let [result (decode (read-until-ready in))]
          (is (nil? (:error result)))
          (is (= [["4294967295"]] (:rows result)))
          (is (= 4 (:typlen (first (:columns result))))))))))

(deftest declared-object-oids-and-arrays-read-their-input-types
  (doseq [[oid value expected] [[2205 "pg_catalog.pg_class" "pg_class"]
                                [4089 "pg_catalog" "pg_catalog"]
                                [2206 "integer[]" "integer[]"]
                                [2211 "{int4,text,-}" "{integer,text,-}"]
                                [2210 "{pg_class,-}" "{pg_class,-}"]
                                [4090 "{public,pg_catalog}" "{public,pg_catalog}"]]]
    (let [r (parse-describe-execute "SELECT $1" [oid] [value])]
      (is (nil? (:error r)))
      (is (= [oid] (:param-oids r)))
      (is (= [[expected]] (:rows r)))
      (when (contains? #{2205 2206 4089} oid)
        (is (= 4 (:typlen (first (:columns r))))))))
  (is (= "42P01" (get-in (parse-describe-execute "SELECT 1" [2210] ["{missing_object}"])
                         [:error \C])))
  (is (= "3F000" (get-in (parse-describe-execute "SELECT 1" [4089] ["missing_schema"])
                         [:error \C]))))

(deftest unused-declared-regtype-parameters-validate-input
  (is (= "42704" (get-in (parse-describe-execute "SELECT 1" [2206] ["missing_type"])
                         [:error \C])))
  (is (= "42704" (get-in (parse-describe-execute "SELECT 1" [2211] ["{missing_type}"])
                         [:error \C]))))

(deftest declared-object-input-after-autocommit-uses-the-current-catalog
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr "CREATE TABLE declared_before (v integer)"))
      (read-until-ready in)
      (let [object (objects/object-by-identity @*conn* objects/pg-class-oid 2200 "declared_before")]
        (d/transact *conn* (objects/rename-object-tx @*conn* objects/pg-class-oid
                                                     (:datahike.pg.object/oid object) "declared_after" nil)))
      (send-msg! out \P (ba (cstr "object_input") (cstr "SELECT $1") [:i16 1] [:i32 2205]))
      (send-bind-execute-sync! out "object_input" ["declared_after"])
      (let [result (decode (read-until-ready in))]
        (is (nil? (:error result)))
        (is (= [["declared_after"]] (:rows result)))))))

(deftest routine-parameter-input-error-aborts-the-transaction
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr "BEGIN"))
      (read-until-ready in)
      (send-msg! out \P (ba (cstr "routine_input") (cstr "SELECT $1")
                            [:i16 1] [:i32 24]))
      (send-bind-execute-sync! out "routine_input" ["abs"])
      (is (= "42725" (get-in (decode (read-until-ready in)) [:error \C])))
      (send-msg! out \Q (cstr "SELECT 1"))
      (is (= "25P02" (get-in (decode (read-until-ready in)) [:error \C])))
      (send-msg! out \Q (cstr "ROLLBACK"))
      (read-until-ready in))))

(deftest undeclared-param-still-defaults-to-text
  (testing "a 0 slot resolves to TEXT — never 0, which sends asyncpg into
            infinite type introspection"
    (is (= [oid-text]
           (:param-oids (parse-describe-execute "SELECT $1" [0] ["55"]))))))

(deftest declared-oids-are-positional
  (testing "$2 and $1 keep their own declarations"
    (is (= [oid-int2 oid-varchar]
           (:param-oids (parse-describe-execute "SELECT $2, $1"
                                                [oid-int2 oid-varchar]
                                                ["55" "hi"]))))))

;; ---------------------------------------------------------------------------
;; RowDescription for a bare `$N` output column
;; ---------------------------------------------------------------------------

(deftest bare-param-select-item-takes-declared-type
  (testing "SELECT $1 is typed from the declaration (issue #27's report)"
    (doseq [[oid typlen] [[oid-int2 2] [oid-int4 4] [oid-int8 8]
                          [oid-float4 4] [oid-float8 8] [oid-bool 1]
                          [oid-date 4] [oid-numeric -1] [oid-varchar -1]]]
      ;; A date parameter must be a DATE: PostgreSQL rejects `1` at
      ;; Bind, and so do we since text temporal parameters are read by
      ;; their input function.
      (let [r (parse-describe-execute "SELECT $1" [oid] [(if (= oid oid-date) "2024-01-15" "1")])]
        (is (= oid (:oid (first (:columns r))))
            (str "SELECT $1 declared " oid " should describe as " oid))
        (is (= typlen (:typlen (first (:columns r))))
            (str "typlen for OID " oid " should be " typlen))))))

(deftest bare-param-select-item-value-round-trips
  (testing "the described type does not change the value we send"
    (let [r (parse-describe-execute "SELECT $1" [oid-int2] ["55"])]
      (is (= [["55"]] (:rows r)))
      (is (= "SELECT 1" (:command-complete r))))))

(deftest parenthesised-param-takes-declared-type
  (testing "PG's exprType sees through parens"
    (is (= oid-int2
           (:oid (first (:columns (parse-describe-execute
                                   "SELECT ($1)" [oid-int2] ["55"]))))))))

(deftest cast-overrides-declared-param-type
  (testing "$1::int4 is int4 even when the client declared text — the
            cast types the expression, the declaration only types the
            parameter"
    (let [r (parse-describe-execute "SELECT $1::int4" [oid-text] ["55"])]
      (is (= [oid-text] (:param-oids r)))
      (is (= oid-int4 (:oid (first (:columns r))))))))

(deftest undeclared-bare-param-falls-back-to-text
  (testing "nothing to resolve from -> TEXT, as before"
    (is (= oid-text
           (:oid (first (:columns (parse-describe-execute
                                   "SELECT $1" [0] ["55"]))))))))

(deftest aliased-bare-param-keeps-alias-and-type
  (testing "SELECT $1 AS a"
    (let [c (first (:columns (parse-describe-execute
                              "SELECT $1 AS a" [oid-int2] ["55"])))]
      (is (= "a" (:name c)))
      (is (= oid-int2 (:oid c))))))

;; ---------------------------------------------------------------------------
;; The pgjdbc executeBatch case (PR #30 follow-up 3)
;; ---------------------------------------------------------------------------

(deftest declared-varchar-on-text-column-is-not-narrowed-to-text
  (testing "setString resolves to varchar(1043); answering text(25) makes
            pgjdbc abort a batch with 'Can't change resolved type for param'"
    (simple "CREATE TABLE note (id int PRIMARY KEY, title text)")
    (let [r (parse-describe-execute
             "INSERT INTO note (id, title) VALUES ($1, $2)"
             [oid-int4 oid-varchar] ["1" "a"])]
      (is (= [oid-int4 oid-varchar] (:param-oids r)))
      (is (= "INSERT 0 1" (:command-complete r))))
    (testing "and the same holds for the ON CONFLICT form ORMs emit"
      (let [r (parse-describe-execute
               (str "INSERT INTO note (id, title) VALUES ($1, $2) "
                    "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title")
               [oid-int4 oid-varchar] ["1" "b"])]
        (is (= [oid-int4 oid-varchar] (:param-oids r)))))
    (is (= [["1" "b"]] (:rows (simple "SELECT id, title FROM note"))))))

(deftest inference-still-applies-to-undeclared-params
  (testing "a client that declares nothing (node-postgres) keeps getting
            the column-derived OIDs"
    (simple "CREATE TABLE note2 (id int PRIMARY KEY, title text)")
    (let [r (parse-describe-execute
             "INSERT INTO note2 (id, title) VALUES ($1, $2)"
             [0 0] ["7" "x"])]
      (is (= [oid-int4 oid-text] (:param-oids r))
          "the int column's own OID, the text column's own OID")
      (is (= "INSERT 0 1" (:command-complete r))))))

;; ---------------------------------------------------------------------------
;; Parse/Bind lifecycle matrix
;; ---------------------------------------------------------------------------

(def declared-int4-expression-cases
  [{:label "bare parameter"
    :sql "SELECT $1"
    :column-oid oid-int4
    :rows [["7"]]}
   {:label "parentheses"
    :sql "SELECT ($1)"
    :column-oid oid-int4
    :rows [["7"]]}
   {:label "unary operator"
    :sql "SELECT -($1)"
    :column-oid oid-int4
    :rows [["-7"]]}
   {:label "left binary operand"
    :sql "SELECT $1 + 1"
    :column-oid oid-int4
    :rows [["8"]]}
   {:label "right binary operand"
    :sql "SELECT 1 + $1"
    :column-oid oid-int4
    :rows [["8"]]}
   {:label "strict scalar function"
    :sql "SELECT abs($1)"
    :column-oid oid-int4
    :rows [["7"]]}
   {:label "common-type function"
    :sql "SELECT coalesce($1, 9)"
    :column-oid oid-int4
    :rows [["7"]]}
   {:label "CASE result"
    :sql "SELECT CASE WHEN true THEN $1 ELSE 9 END"
    :column-oid oid-int4
    :rows [["7"]]}
   {:label "array element"
    :sql "SELECT ARRAY[$1, 9]"
    :column-oid oid-int4-array
    :rows [["{7,9}"]]}
   {:label "polymorphic introspection"
    :sql "SELECT pg_typeof($1)::text"
    :column-oid oid-text
    :rows [["integer"]]}
   {:label "NULL predicate"
    :sql "SELECT $1 IS NULL"
    :column-oid oid-bool
    :rows [["f"]]}])

(deftest declared-parameter-types-drive-expression-lifecycle
  (doseq [{:keys [label sql column-oid rows]} declared-int4-expression-cases]
    (testing label
      (let [r (parse-describe-execute sql [oid-int4] ["7"])]
        (is (nil? (:error r)) (str sql " returned " (:error r)))
        (is (= [oid-int4] (:param-oids r)) sql)
        (is (= column-oid (:oid (first (:columns r)))) sql)
        (is (= rows (:rows r)) sql)))))

(def declared-type-lifecycle-cases
  [{:label "bool under NOT"
    :sql "SELECT NOT $1"
    :param-oid oid-bool
    :value "true"
    :column-oid oid-bool
    :rows [["f"]]}
   {:label "int2 arithmetic"
    :sql "SELECT $1 + 1::int2"
    :param-oid oid-int2
    :value "7"
    :column-oid oid-int2
    :rows [["8"]]}
   {:label "int8 arithmetic"
    :sql "SELECT $1 + 1"
    :param-oid oid-int8
    :value "2147483648"
    :column-oid oid-int8
    :rows [["2147483649"]]}
   {:label "numeric arithmetic"
    :sql "SELECT $1 + 1.25"
    :param-oid oid-numeric
    :value "7.50"
    :column-oid oid-numeric
    :rows [["8.75"]]}
   {:label "float4 arithmetic"
    :sql "SELECT $1 + 1::float4"
    :param-oid oid-float4
    :value "7.5"
    :column-oid oid-float4
    :rows [["8.5"]]}
   {:label "float8 arithmetic"
    :sql "SELECT $1 + 1"
    :param-oid oid-float8
    :value "7.5"
    :column-oid oid-float8
    :rows [["8.5"]]}
   {:label "text function"
    :sql "SELECT upper($1)"
    :param-oid oid-text
    :value "hello"
    :column-oid oid-text
    :rows [["HELLO"]]}
   {:label "varchar argument with text result"
    :sql "SELECT upper($1)"
    :param-oid oid-varchar
    :value "hello"
    :column-oid oid-text
    :rows [["HELLO"]]}
   {:label "date arithmetic"
    :sql "SELECT $1 + 1"
    :param-oid oid-date
    :value "2024-01-01"
    :column-oid oid-date
    :rows [["2024-01-02"]]}])

(deftest declared-types-drive-bind-decoding-and-result-metadata
  (doseq [{:keys [label sql param-oid value column-oid rows]}
          declared-type-lifecycle-cases]
    (testing label
      (let [r (parse-describe-execute sql [param-oid] [value])]
        (is (nil? (:error r)) (str sql " returned " (:error r)))
        (is (= [param-oid] (:param-oids r)) sql)
        (is (= column-oid (:oid (first (:columns r)))) sql)
        (is (= rows (:rows r)) sql)))))

(deftest contextual-inference-resolves-unknown-parameter-slots
  (doseq [{:keys [label sql param-oid column-oid rows]}
          [{:label "explicit cast"
            :sql "SELECT $1::int4"
            :column-oid oid-int4
            :rows [["7"]]}
           {:label "binary operator"
            :sql "SELECT $1 + 1"
            :column-oid oid-int4
            :rows [["8"]]}
           {:label "common-type function"
            :sql "SELECT coalesce($1, 9)"
            :column-oid oid-int4
            :rows [["7"]]}
           {:label "comparison operator"
            :sql "SELECT $1 = 7"
            :column-oid oid-bool
            :rows [["t"]]}
           {:label "text concatenation"
            :sql "SELECT $1 || 'x'"
            :param-oid oid-text
            :column-oid oid-text
            :rows [["7x"]]}
           {:label "common-type greatest"
            :sql "SELECT greatest($1, 9)"
            :column-oid oid-int4
            :rows [["9"]]}]]
    (testing label
      (let [param-oid (or param-oid oid-int4)
            r (parse-describe-execute sql [0] ["7"])]
        (is (nil? (:error r)) (str sql " returned " (:error r)))
        (is (= [param-oid] (:param-oids r)) sql)
        (is (= column-oid (:oid (first (:columns r)))) sql)
        (is (= rows (:rows r)) sql)))))

(deftest parsed-statement-can-be-rebound-with-values-and-null
  (let [[first-bind second-bind null-bind]
        (parse-describe-execute-many "SELECT $1 + 1" [oid-int4]
                                     [["2"] ["8"] [nil]])]
    (is (= [oid-int4] (:param-oids first-bind)))
    (is (= oid-int4 (:oid (first (:columns first-bind)))))
    (is (= [["3"]] (:rows first-bind)))
    (is (= [["9"]] (:rows second-bind)))
    (is (= [[nil]] (:rows null-bind)))
    (is (every? #(nil? (:error %)) [first-bind second-bind null-bind]))))

(deftest the-wire-scanners-see-what-postgresql-sees
  ;; pgjdbc splits a statement string and rejects an unterminated comment
  ;; itself, so the server's own scanners are only reached raw. Every
  ;; expectation is PostgreSQL 17's for the same message.
  (testing "one Query message: semicolons inside E'' and dollar quotes do not split it"
    ;; `\'` does not end an E'' literal, so this is two statements, not
    ;; three; the splitter used to cut the first at its inner `;`.
    (let [r (simple "SELECT E'a\\'b;c'; SELECT $q$x;y$q$, '/*z*/'")]
      (is (nil? (:error r)))
      (is (= [["a'b;c"] ["x;y" "/*z*/"]] (:rows r)))))
  (testing "a block comment that never closes is 42601, not a shorter query"
    ;; It was stripped to the end of the text, so `SELECT 1 /* oops` ran
    ;; as `SELECT 1`. The nested one never closes either -- its inner
    ;; `/*` opens a second level -- and answered an empty query.
    (doseq [sql ["SELECT 1 /* oops" "/* dir/*.sql */ SELECT 1"]]
      (let [r (simple sql)]
        (is (= "42601" (get-in r [:error \C])) sql)
        (is (= (str "unterminated /* comment at or near \"" (subs sql (str/index-of sql "/*")) "\"")
               (get-in r [:error \M]))
            sql)
        (is (nil? (:rows r)) sql)))
    (testing "and through Parse"
      (let [r (parse-describe-execute "SELECT 1 /* oops" [] [])]
        (is (= "42601" (get-in r [:error \C])))))))

(defn- send-copy-data! [out ^String data]
  (send-msg! out \d (.getBytes data StandardCharsets/UTF_8))
  (send-msg! out \c (byte-array 0)))

(defn- ready-status [messages]
  (some (fn [[tag ^bytes body]] (when (= \Z tag) (char (aget body 0))))
        messages))

(deftest postgres-simple-query-resumes-after-each-copy
  (simple "CREATE TABLE copy_group (id int PRIMARY KEY)")
  (with-conn
    (fn [in out]
      (send-msg! out \Q
                 (cstr (str "SELECT 0; COPY copy_group FROM STDIN;"
                            " COPY copy_group FROM STDIN; SELECT count(*) FROM copy_group")))
      (let [prefix (read-until-tags in #{\G \Z})]
        (is (= [\T \D \C \G] (mapv first prefix)))
        (is (= [["0"]] (:rows (decode prefix)))))
      (send-copy-data! out "1\n")
      (let [next-copy (read-until-tags in #{\G \Z})]
        (is (= [\C \G] (mapv first next-copy)))
        (is (= "COPY 1" (:command-complete (decode next-copy))))
        (is (= [["0"]] (:rows (simple "SELECT count(*) FROM copy_group"))))
        (when (= \G (ffirst (rseq next-copy)))
          (send-copy-data! out "2\n")
          (let [suffix (read-until-ready in)]
            (is (= [\C \T \D \C \Z] (mapv first suffix)))
            (is (= "COPY 1" (:command-complete (decode (take 1 suffix)))))
            (is (= [["2"]] (:rows (decode suffix))))
            (is (= \I (ready-status suffix))))))))
  (is (= [["1"] ["2"]] (:rows (simple "SELECT id FROM copy_group ORDER BY id")))))

(deftest simple-query-copy-failures-roll-back-the-group
  (simple "CREATE TABLE copy_group (id int PRIMARY KEY)")
  (doseq [[label tail finish! sqlstate]
          [[:copy-row-error "COPY copy_group FROM STDIN; SELECT 42"
            #(send-copy-data! % "not-an-integer\n") "22P02"]
           [:copy-fail "COPY copy_group FROM STDIN; SELECT 42"
            #(send-msg! % \f (cstr "copy canceled by test")) "57014"]
           [:suffix-error "SELECT 1 / 0; SELECT 42" nil "22012"]]]
    (testing (name label)
      (with-conn
        (fn [in out]
          (send-msg! out \Q (cstr (str "INSERT INTO copy_group VALUES (1);"
                                       " COPY copy_group FROM STDIN; " tail)))
          (is (= [\C \G] (mapv first (read-until-tags in #{\G \Z}))))
          (send-copy-data! out "2\n")
          (let [result (if finish!
                         (let [next-copy (read-until-tags in #{\G \Z})]
                           (is (= [\C \G] (mapv first next-copy)))
                           (if (= \G (ffirst (rseq next-copy)))
                             (do (finish! out) (read-until-ready in))
                             next-copy))
                         (read-until-ready in))]
            (is (= sqlstate (get-in (decode result) [:error \C])))
            (is (nil? (:rows (decode result))))
            (is (= \I (ready-status result))))
          (send-msg! out \Q (cstr "SELECT count(*) FROM copy_group"))
          (is (= [["0"]] (:rows (decode (read-until-ready in))))))))))

(deftest simple-query-copy-error-keeps-an-explicit-transaction-aborted
  (simple "CREATE TABLE copy_group (id int PRIMARY KEY)")
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr "BEGIN; COPY copy_group FROM STDIN; SELECT 1 / 0"))
      (is (= [\C \G] (mapv first (read-until-tags in #{\G \Z}))))
      (send-copy-data! out "1\n")
      (let [result (read-until-ready in)]
        (is (= "22012" (get-in (decode result) [:error \C])))
        (is (= \E (ready-status result))))
      (send-msg! out \Q (cstr "SELECT * FROM copy_group"))
      (is (= "25P02" (get-in (decode (read-until-ready in)) [:error \C])))
      (send-msg! out \Q (cstr "ROLLBACK"))
      (is (= \I (ready-status (read-until-ready in))))))
  (is (= [] (or (:rows (simple "SELECT id FROM copy_group")) []))))

(deftest simple-query-copy-can-transition-to-an-explicit-transaction
  (simple "CREATE TABLE copy_group (id int PRIMARY KEY)")
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr "COPY copy_group FROM STDIN; BEGIN"))
      (is (= [\G] (mapv first (read-until-tags in #{\G \Z}))))
      (send-copy-data! out "1\n")
      (is (= \T (ready-status (read-until-ready in))))
      (is (= [["0"]] (:rows (simple "SELECT count(*) FROM copy_group"))))
      (send-msg! out \Q (cstr "ROLLBACK"))
      (is (= \I (ready-status (read-until-ready in))))))
  (is (= [["0"]] (:rows (simple "SELECT count(*) FROM copy_group")))))

(deftest disconnect-during-a-later-copy-discards-the-group
  (simple "CREATE TABLE copy_group (id int PRIMARY KEY)")
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr "COPY copy_group FROM STDIN; COPY copy_group FROM STDIN"))
      (is (= [\G] (mapv first (read-until-tags in #{\G \Z}))))
      (send-copy-data! out "1\n")
      (is (= [\C \G] (mapv first (read-until-tags in #{\G \Z}))))))
  (is (= [["0"]] (:rows (simple "SELECT count(*) FROM copy_group")))))
