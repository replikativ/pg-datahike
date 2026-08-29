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
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
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
        (binding [*port* (.getPort server)]
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

(defn- bind-body-formats [portal-name statement-name params result-formats]
  (apply ba (cstr portal-name) (cstr statement-name)
         [:i16 0]                    ; all params text
         [:i16 (count params)]
         (concat
          (mapcat (fn [^String p]
                    (if (nil? p)
                      [[:i32 -1]]
                      (let [b (.getBytes p StandardCharsets/UTF_8)]
                        [[:i32 (alength b)] b])))
                  params)
          [[:i16 (count result-formats)]]
          (map (fn [format] [:i16 format]) result-formats))))

(defn- bind-body [statement-name params]
  (bind-body-formats "" statement-name params []))

(defn- send-bind-execute-sync! [^DataOutputStream out statement-name params]
  (send-msg! out \B (bind-body statement-name params))
  (send-msg! out \E (ba (cstr "") [:i32 0]))
  (send-msg! out \S (byte-array 0)))

(defn- read-until-tag [^DataInputStream in terminal]
  (loop [acc []]
    (let [[tag body :as msg] (read-msg in)
          acc (conj acc msg)]
      (if (= terminal tag) acc (recur acc)))))

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
            (send-msg! out \S (byte-array 0))
            (read-until-ready in)
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
      (is (= [oid] (:param-oids (parse-describe-execute "SELECT $1" [oid] ["1"])))
          (str "declared OID " oid " should survive Describe")))))

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
      (let [r (parse-describe-execute "SELECT $1" [oid] ["1"])]
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
