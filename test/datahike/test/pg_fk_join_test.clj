(ns datahike.test.pg-fk-join-test
  "FK-via-ref JOIN rewrite: on a native Datahike schema where
   :db.type/ref attrs store the target entity-id, SQL JOIN on
   `a.fk = b.pk` should resolve by entity identity even though
   `a.fk` surfaces as the entity-id and `b.pk` is the user's business
   key. Covers both the auto-detected case (RHS is :db.unique/identity)
   and the hint-driven case (:datahike.pg/references override)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.schema :as pgs]
            [datahike.pg.server :as pg])
  (:import [java.io StringReader]
           [java.sql Connection DriverManager SQLException Statement]
           [org.postgresql PGConnection]
           [org.postgresql.copy CopyManager]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *port* nil)

(defn fk-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn
                  [{:db/ident :company/id
                    :db/valueType :db.type/long
                    :db/cardinality :db.cardinality/one
                    :db/unique :db.unique/identity}
                   {:db/ident :company/name
                    :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one}
                   {:db/ident :person/id
                    :db/valueType :db.type/long
                    :db/cardinality :db.cardinality/one
                    :db/unique :db.unique/identity}
                   {:db/ident :person/name
                    :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one}
                   {:db/ident :person/company
                    :db/valueType :db.type/ref
                    :db/cardinality :db.cardinality/one}])
      (d/transact conn
                  [{:company/id 1 :company/name "Acme"}
                   {:company/id 2 :company/name "Globex"}])
      (d/transact conn
                  [{:person/id 1 :person/name "Alice"   :person/company [:company/id 1]}
                   {:person/id 2 :person/name "Bob"     :person/company [:company/id 1]}
                   {:person/id 3 :person/name "Charlie" :person/company [:company/id 2]}])
      (let [{:keys [server]} (pg/start-server {"fk" conn} {:port 0})]
        (try
          (binding [*conn* conn *port* (.getPort server)]
            (f))
          (finally
            (.stop server)
            (d/release conn)
            (d/delete-database cfg)))))))

(use-fixtures :each fk-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/fk?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- pairs [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (loop [acc []]
      (if (.next rs)
        (recur (conj acc [(.getString rs 1) (.getString rs 2)]))
        acc))))

(deftest fk-via-ref-auto-inner-join
  (with-open [c (jdbc)]
    (is (= [["Alice"   "Acme"]
            ["Bob"     "Acme"]
            ["Charlie" "Globex"]]
           (pairs c "SELECT p.name, c.name FROM person p JOIN company c
                     ON p.company = c.id ORDER BY p.name")))))

(deftest db-id-join-still-works
  ;; Regression: the explicit db_id JOIN path must remain functional —
  ;; it's used by DDL-created tables and by any code that reaches into
  ;; the synthetic db_id column directly.
  (with-open [c (jdbc)]
    (is (= [["Alice"   "Acme"]
            ["Bob"     "Acme"]
            ["Charlie" "Globex"]]
           (pairs c "SELECT p.name, c.name FROM person p JOIN company c
                     ON p.company = c.db_id ORDER BY p.name")))))

(deftest fk-via-ref-hint-wins-over-auto
  ;; Even when the FK target table has multiple unique attrs, an explicit
  ;; :datahike.pg/references hint picks one. We prove this by adding a
  ;; second unique attr to company and verifying the hint-chosen one
  ;; still drives the JOIN.
  (d/transact *conn*
              [{:db/ident :company/tax_id
                :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one
                :db/unique :db.unique/identity}])
  (pgs/set-hint! *conn* :person/company {:references :company/id})
  (with-open [c (jdbc)]
    (is (= [["Alice"   "Acme"]
            ["Bob"     "Acme"]
            ["Charlie" "Globex"]]
           (pairs c "SELECT p.name, c.name FROM person p JOIN company c
                     ON p.company = c.id ORDER BY p.name")))))

(deftest where-on-joined-table
  (with-open [c (jdbc)]
    (is (= [["Charlie" "Globex"]]
           (pairs c "SELECT p.name, c.name FROM person p JOIN company c
                     ON p.company = c.id
                     WHERE c.name = 'Globex' ORDER BY p.name")))))

(deftest a-left-join-onto-a-ref-target-keeps-every-left-row
  ;; `person.company` is a :db.type/ref; the join reaches the target by
  ;; its entity id. The lowering was written for the other direction --
  ;; the ref OWNER on the joined side, as in `FROM transaction LEFT JOIN
  ;; posting ON posting.transaction = t.db_id` -- and took the joined
  ;; alias to be the ref's owner either way, emitting a pattern whose
  ;; entity and value are the same variable. The join then answered
  ;; NOTHING AT ALL, for every row.
  (with-open [c (jdbc)]
    (is (= [["Alice"   "Acme"]
            ["Bob"     "Acme"]
            ["Charlie" "Globex"]]
           (pairs c "SELECT p.name, c.name FROM person p LEFT JOIN company c
                       ON p.company = c.db_id
                     ORDER BY p.name")))
    (testing "a person without a company is kept, with NULLs"
      (d/transact *conn* [{:person/id 4 :person/name "Dana"}])
      (is (= [["Alice"   "Acme"]
              ["Bob"     "Acme"]
              ["Charlie" "Globex"]
              ["Dana"    nil]]
             (pairs c "SELECT p.name, c.name FROM person p LEFT JOIN company c
                         ON p.company = c.db_id
                       ORDER BY p.name"))))
    (testing "every ON condition applies here too"
      (is (= [["Alice"   "Acme"]
              ["Bob"     "Acme"]
              ["Charlie" nil]
              ["Dana"    nil]]
             (pairs c "SELECT p.name, c.name FROM person p LEFT JOIN company c
                         ON p.company = c.db_id AND c.name = 'Acme'
                       ORDER BY p.name"))))))

(deftest update-ref-to-a-transaction-local-row-survives-replay
  (d/transact *conn* [{:db/ident :replay_doc/id :db/valueType :db.type/long
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :replay_doc/link :db/valueType :db.type/ref
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :replay_target/id :db/valueType :db.type/long
                       :db/cardinality :db.cardinality/one}])
  (d/transact *conn* [{:replay_doc/id 1}])
  (with-open [a (jdbc)
              b (jdbc)
              sa (.createStatement a)
              sb (.createStatement b)]
    (.execute sa "BEGIN")
    (.executeUpdate sa "INSERT INTO replay_target (id) VALUES (10)")
    (let [provisional-id (with-open [rs (.executeQuery sa "SELECT db_id FROM replay_target WHERE id = 10")]
                           (.next rs)
                           (.getLong rs 1))]
      (is (= 1 (.executeUpdate sa (str "UPDATE replay_doc SET link = " provisional-id " WHERE id = 1"))))
      (.executeUpdate sb "INSERT INTO replay_target (id) VALUES (20)")
      (.execute sa "COMMIT")
      (is (= [["1" "10"]]
             (pairs a (str "SELECT d.id, t.id FROM replay_doc d JOIN replay_target t "
                           "ON d.link = t.db_id WHERE d.id = 1")))))))

(deftest insert-and-copy-resolve-native-reference-keys-on-the-writer
  (d/transact *conn* [{:db/ident :replay_doc/id :db/valueType :db.type/long
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :replay_doc/link :db/valueType :db.type/ref
                       :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                      {:db/ident :replay_doc/input_id :db/valueType :db.type/long
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :replay_target/id :db/valueType :db.type/long
                       :db/cardinality :db.cardinality/one}])
  (doseq [[operation target-id decoy-id] [[:insert 10 20] [:copy 30 40]]]
    (testing (name operation)
      (with-open [a (jdbc)
                  b (jdbc)
                  sa (.createStatement a)
                  sb (.createStatement b)]
        (.execute sa "BEGIN")
        (.executeUpdate sa (str "INSERT INTO replay_target (id) VALUES (" target-id ")"))
        (let [provisional-id
              (with-open [rows (.executeQuery sa (str "SELECT db_id FROM replay_target WHERE id = " target-id))]
                (.next rows)
                (.getLong rows 1))]
          (case operation
            :insert
            (is (= 1 (.executeUpdate sa (str "INSERT INTO replay_doc VALUES ("
                                             target-id "," provisional-id "," provisional-id ")"))))
            :copy
            (let [manager (CopyManager. (.unwrap a PGConnection))]
              (with-open [input (StringReader. (str target-id "\t" provisional-id "\t" provisional-id "\n"))]
                (is (= 1 (.copyIn manager "COPY replay_doc (id, link, input_id) FROM STDIN" input))))))
          ;; The decoy occupies the original entity ID and uses it as a
          ;; unique reference key. A check against the stale key would
          ;; reject the original INSERT or COPY instead of replaying it.
          (.executeUpdate sb (str "INSERT INTO replay_target (id) VALUES (" decoy-id ")"))
          (is (= [(str decoy-id)]
                 (mapv first (pairs b (str "SELECT id, db_id FROM replay_target WHERE db_id = " provisional-id)))))
          (.executeUpdate sb (str "INSERT INTO replay_doc VALUES (" decoy-id "," provisional-id ",0)"))
          (.execute sa "COMMIT")
          (is (= #{[target-id target-id provisional-id] [decoy-id decoy-id 0]}
                 (d/q '[:find ?id ?linked-id ?input-id
                        :in $ [?id ...]
                        :where [?doc :replay_doc/id ?id]
                        [?doc :replay_doc/link ?link]
                        [?doc :replay_doc/input_id ?input-id]
                        [?link :replay_target/id ?linked-id]]
                      (d/db *conn*) [target-id decoy-id]))))))))

(defn- statement-state [^Statement statement sql]
  (try
    (.execute statement sql)
    nil
    (catch SQLException exception
      (.getSQLState exception))))

(deftest deleting-a-native-reference-target-records-the-inbound-write
  (with-open [a (jdbc)
              b (jdbc)
              sa (.createStatement a)
              sb (.createStatement b)]
    (.execute sa "BEGIN")
    (is (= 1 (.executeUpdate sa "UPDATE person SET company = 2 WHERE id = 1")))
    (is (= 1 (.executeUpdate sb "DELETE FROM company WHERE id = 1")))
    (is (= [["Alice" nil]]
           (pairs b "SELECT name, company FROM person WHERE id = 1")))
    (is (= "40001" (statement-state sa "COMMIT")))
    (is (= [["Alice" nil]]
           (pairs b "SELECT name, company FROM person WHERE id = 1")))))

(defn- check-inbound-reference-conflict [next-command]
  (with-open [a (jdbc)
              b (jdbc)
              sa (.createStatement a)
              sb (.createStatement b)]
    (.execute sa "BEGIN")
    (is (= 1 (.executeUpdate sa "DELETE FROM company WHERE id = 1")))
    (is (= 1 (.executeUpdate sb "UPDATE person SET company = 1 WHERE id = 3")))
    (is (= "40001" (statement-state sa next-command)))
    (.execute sa "ROLLBACK")
    (is (= [["Charlie" "Acme"]]
           (pairs b (str "SELECT p.name, c.name FROM person p JOIN company c "
                         "ON p.company = c.db_id WHERE p.id = 3"))))))

(deftest commit-does-not-remove-a-concurrently-written-inbound-reference
  (check-inbound-reference-conflict "COMMIT"))

(deftest rebase-does-not-remove-a-concurrently-written-inbound-reference
  (check-inbound-reference-conflict "UPDATE person SET name = 'Alice updated' WHERE id = 1"))

(deftest replay-preserves-a-concurrent-reference-to-a-different-target
  (with-open [a (jdbc)
              b (jdbc)
              sa (.createStatement a)
              sb (.createStatement b)]
    (.execute sa "BEGIN")
    (is (= 1 (.executeUpdate sa "DELETE FROM company WHERE id = 1")))
    (is (= 1 (.executeUpdate sb "UPDATE person SET company = 2 WHERE id = 1")))
    (.execute sa "COMMIT")
    (is (= [["Alice" "Globex"]]
           (pairs b (str "SELECT p.name, c.name FROM person p JOIN company c "
                         "ON p.company = c.db_id WHERE p.id = 1"))))))

(defn- install-component! [row]
  (d/transact *conn* [{:db/ident :person/component :db/valueType :db.type/ref
                       :db/cardinality :db.cardinality/one :db/isComponent true}
                      {:db/ident :component_data/id :db/valueType :db.type/long
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :component_data/note :db/valueType :db.type/string
                       :db/cardinality :db.cardinality/one}])
  (d/transact *conn* [{:db/id [:person/id 1] :person/component row}]))

(deftest deleting-a-component-conflicts-with-a-new-attribute-on-it
  (install-component! {:component_data/id 10})
  (with-open [a (jdbc)
              b (jdbc)
              sa (.createStatement a)
              sb (.createStatement b)]
    (.execute sa "BEGIN")
    (is (= 1 (.executeUpdate sa "UPDATE component_data SET note = 'changed' WHERE id = 10")))
    (is (= 1 (.executeUpdate sb "DELETE FROM person WHERE id = 1")))
    (is (= "40001" (statement-state sa "COMMIT")))
    (is (= [] (pairs b "SELECT id, note FROM component_data")))))

(deftest replayed-component-deletion-conflicts-with-a-concurrent-null-assignment
  (install-component! {:component_data/id 10 :component_data/note "old"})
  (with-open [a (jdbc)
              b (jdbc)
              sa (.createStatement a)
              sb (.createStatement b)]
    (.execute sa "BEGIN")
    (is (= 1 (.executeUpdate sa "DELETE FROM person WHERE id = 1")))
    (is (= 1 (.executeUpdate sb "UPDATE component_data SET note = NULL WHERE id = 10")))
    (is (= "40001" (statement-state sa "COMMIT")))
    (is (= [["10" nil]] (pairs b "SELECT id, note FROM component_data")))))

(deftest replay-rechecks-the-basis-at-the-writer
  (with-open [a (jdbc)
              sa (.createStatement a)]
    (.execute sa "BEGIN")
    (is (= 1 (.executeUpdate sa "DELETE FROM company WHERE id = 1")))
    (let [transact-recorded @#'pg/transact-recorded!]
      (with-redefs-fn {#'pg/transact-recorded!
                       (fn [conn operations options]
                         (d/transact conn [[:db/add [:person/id 3] :person/company [:company/id 1]]])
                         (transact-recorded conn operations options))}
        #(is (= "40001" (statement-state sa "COMMIT")))))
    (is (= [["Charlie" "Acme"]]
           (pairs a (str "SELECT p.name, c.name FROM person p JOIN company c "
                         "ON p.company = c.db_id WHERE p.id = 3"))))))
