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
  (:import [datahike.pg PgWireServer PgWireServer$QueryHandlerFactory]
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
                                   _fmt (.getShort bb)]
                               {:name nm :oid oid :typlen typlen
                                :typmod typmod}))))))
       ;; DataRow
       \D (let [bb (buf body)
                n (.getShort bb)
                row (vec (repeatedly
                          n (fn []
                              (let [len (.getInt bb)]
                                (when-not (neg? len)
                                  (let [b (byte-array len)]
                                    (.get bb b)
                                    (String. b StandardCharsets/UTF_8)))))))]
            (update acc :rows (fnil conj []) row))
       \n (assoc acc :no-data? true)
       \C (assoc acc :command-complete
                 (String. body 0 (dec (alength body)) StandardCharsets/UTF_8))
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

(defn- parse-describe-execute
  "Parse `sql` declaring `param-oids`, Describe the statement, Bind
   `params` (text format) and Execute. Returns the decoded messages."
  ([sql param-oids params] (parse-describe-execute sql param-oids params nil))
  ([sql param-oids params _opts]
   (with-conn
     (fn [in out]
       (send-msg! out \P (apply ba (cstr "") (cstr sql)
                                [:i16 (count param-oids)]
                                (map (fn [o] [:i32 o]) param-oids)))
       (send-msg! out \D (ba (.getBytes "S" StandardCharsets/UTF_8) (cstr "")))
       (send-msg! out \B (apply ba (cstr "") (cstr "")
                                [:i16 0]                    ; all params text
                                [:i16 (count params)]
                                (concat
                                 (mapcat (fn [^String p]
                                           (if (nil? p)
                                             [[:i32 -1]]
                                             (let [b (.getBytes p StandardCharsets/UTF_8)]
                                               [[:i32 (alength b)] b])))
                                         params)
                                 [[:i16 0]])))              ; all results text
       (send-msg! out \E (ba (cstr "") [:i32 0]))
       (send-msg! out \S (byte-array 0))
       (decode (read-until-ready in))))))

(defn- simple [sql]
  (with-conn
    (fn [in out]
      (send-msg! out \Q (cstr sql))
      (decode (read-until-ready in)))))

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
