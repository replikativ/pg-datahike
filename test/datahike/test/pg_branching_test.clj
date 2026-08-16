(ns datahike.test.pg-branching-test
  "End-to-end test for the branching surface exposed through pgwire:

   - `SET datahike.branch = 'X'` / `RESET datahike.branch` — route reads
     against a named branch for the session.
   - `SET datahike.commit_id = '<uuid>'` — pin the session to a specific
     commit (Datahike-unique; no PG equivalent).
   - JDBC URL `/<name>:<branch>` — seed the session's :branch at
     connection time without an explicit SET.
   - `datahike.branches()` / `datahike.current_branch()` /
     `datahike.commit_id()` / `datahike.parent_commits()` — read-only
     introspection.
   - `datahike.create_branch('new','from')` / `datahike.delete_branch('n')`
     — administrative branch ops via SQL."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.versioning :as v]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *port* nil)
(def ^:dynamic *initial-commit-id* nil)

(defn branching-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn
                  [{:db/ident :person/id
                    :db/valueType :db.type/long
                    :db/cardinality :db.cardinality/one
                    :db/unique :db.unique/identity}
                   {:db/ident :person/name
                    :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one}])
      (d/transact conn [{:person/id 1 :person/name "Alice"}])
      (let [initial-cid (get-in (d/db conn) [:meta :datahike/commit-id])]
        ;; Create a branch that keeps the same dataset. Since Datahike's
        ;; branching is O(1) metadata and branch writes-via-pgwire land
        ;; on the default branch (see CHANGELOG — deferred to post-0.1),
        ;; branches in these tests differ only by reachable commits /
        ;; administrative state, not by data content.
        (v/branch! conn :db :feature)
        (let [{:keys [server]} (pg/start-server {"demo" conn} {:port 0})]
          (try
            (binding [*conn* conn
                      *port* (.getPort server)
                      *initial-commit-id* initial-cid]
              (f))
            (finally
              (.stop server)
              (d/release conn)
              (d/delete-database cfg))))))))

(use-fixtures :each branching-fixture)

(defn- ^Connection jdbc
  ([] (jdbc "demo"))
  ([db]
   (DriverManager/getConnection
    (str "jdbc:postgresql://127.0.0.1:" *port* "/" db
         "?user=x&password=x&sslmode=disable&binaryTransfer=false&preferQueryMode=simple"))))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (let [md (.getMetaData rs)
          n  (.getColumnCount md)]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs (inc %)) (range n))))
          acc)))))

(defn- exec [^Connection c sql]
  (with-open [st (.createStatement c)]
    (.execute st sql)))

;; ---------------------------------------------------------------------------
;; Introspection
;; ---------------------------------------------------------------------------

(deftest branches-lists-both
  (with-open [c (jdbc)]
    (is (= [["db"] ["feature"]]
           (rows c "SELECT datahike.branches()")))))

(deftest current-branch-default-is-db
  (with-open [c (jdbc)]
    (is (= [["db"]]
           (rows c "SELECT datahike.current_branch()")))))

(deftest commit-id-is-a-uuid-string
  (with-open [c (jdbc)]
    (let [[[cid]] (rows c "SELECT datahike.commit_id()")]
      (is (some? cid))
      (is (uuid? (java.util.UUID/fromString cid))))))

(deftest parent-commits-one-in-common-case
  (with-open [c (jdbc)]
    (let [ps (rows c "SELECT datahike.parent_commits()")]
      (is (pos? (count ps))))))

;; ---------------------------------------------------------------------------
;; Branch-pinning via SET
;; ---------------------------------------------------------------------------

(deftest set-branch-switches-read-view
  (with-open [c (jdbc)]
    (exec c "SET datahike.branch = 'feature'")
    (is (= [["feature"]] (rows c "SELECT datahike.current_branch()")))
    (exec c "RESET datahike.branch")
    (is (= [["db"]] (rows c "SELECT datahike.current_branch()")))))

(deftest branch-via-url-suffix
  (with-open [c (jdbc "demo:feature")]
    (is (= [["feature"]] (rows c "SELECT datahike.current_branch()")))
    ;; Data is the same on both branches (branch! copied the head),
    ;; but the current_branch report reflects the URL-seeded pin.
    (is (= [["1"]] (rows c "SELECT count(*) FROM person")))))

(deftest set-commit-id-pins-exact-uuid
  (with-open [c (jdbc)]
    ;; Transact another row to move the head; SET commit_id = <initial>
    ;; should see only Alice, not the new row.
    (d/transact *conn* [{:person/id 2 :person/name "Bob"}])
    (is (= 2 (count (rows c "SELECT id FROM person"))))
    (exec c (str "SET datahike.commit_id = '" *initial-commit-id* "'"))
    (is (= 1 (count (rows c "SELECT id FROM person"))))
    (exec c "RESET datahike.commit_id")
    (is (= 2 (count (rows c "SELECT id FROM person"))))))

(deftest set-commit-id-rejects-non-uuid
  (with-open [c (jdbc)]
    (is (thrown? SQLException
                 (exec c "SET datahike.commit_id = 'not-a-uuid'")))))

;; ---------------------------------------------------------------------------
;; Admin ops via SQL
;; ---------------------------------------------------------------------------

(deftest create-and-delete-branch
  (with-open [c (jdbc)]
    (is (not (contains? (set (map first (rows c "SELECT datahike.branches()"))) "preview")))
    (is (= [["preview"]]
           (rows c "SELECT datahike.create_branch('preview', 'db')")))
    (is (contains? (set (map first (rows c "SELECT datahike.branches()"))) "preview"))
    (is (= [["preview"]]
           (rows c "SELECT datahike.delete_branch('preview')")))
    (is (not (contains? (set (map first (rows c "SELECT datahike.branches()"))) "preview")))))

(deftest create-branch-from-commit-uuid
  (with-open [c (jdbc)]
    (testing "create_branch accepts a UUID string as the from-ref"
      (is (= [["from-cid"]]
             (rows c (str "SELECT datahike.create_branch('from-cid', '"
                          *initial-commit-id* "')"))))
      (is (contains? (set (map first (rows c "SELECT datahike.branches()"))) "from-cid")))))
