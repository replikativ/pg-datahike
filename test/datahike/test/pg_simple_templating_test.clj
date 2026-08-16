(ns datahike.test.pg-simple-templating-test
  "Simple-protocol number templating for UPDATE / DELETE.

   template/parameterize-numbers rewrites bare number literals to $N
   so every cache layer keys on one shape per statement family; the
   execute path then runs the statement as a one-shot prepared
   statement (SELECT resolves ParamRefs inline; UPDATE/DELETE carry
   the captured literals into the exec-time *cached-bound* binding,
   which their WHERE re-translation and SET evaluation read).

   These tests pin the semantics of that path at the handler level:
   templated writes must land on exactly the rows the literals name,
   respect BEGIN/COMMIT/ROLLBACK, and every blacklisted (non-
   templatable) statement must keep its untemplated behavior."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.template :as template])
  (:import [datahike.pg PgWireServer$QueryHandler]))

(defn- fresh-handler []
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}
        _ (d/create-database cfg)
        conn (d/connect cfg)]
    {:conn conn :cfg cfg :handler (pg/make-query-handler conn {})}))

(defn- exec [{:keys [^PgWireServer$QueryHandler handler]} sql]
  (.execute handler sql))

(defn- rows [r] (when-not (.error r) (mapv vec (.rows r))))

(defn- tag [r] (.commandTag r))

(defn- release! [{:keys [conn cfg]}]
  (d/release conn)
  (d/delete-database cfg))

(defn- seed-accounts!
  "t(id, bal) with rows 1→100, 2→200, 3→300."
  [h]
  (is (nil? (.error (exec h "CREATE TABLE t(id INTEGER, bal INTEGER)"))))
  (doseq [[id bal] [[1 100] [2 200] [3 300]]]
    (is (nil? (.error (exec h (str "INSERT INTO t(id, bal) VALUES (" id ", " bal ")")))))))

(defn- table-state [h]
  (rows (exec h "SELECT id, bal FROM t ORDER BY id")))

(deftest templated-update-arithmetic
  (testing "UPDATE SET col = col + -N WHERE id = M goes through the templater"
    ;; Premise guard: if the templater stops matching this shape the
    ;; test would silently exercise the plain parse path.
    (is (some? (template/parameterize-numbers
                "UPDATE t SET bal = bal + -30 WHERE id = 2")))
    (let [h (fresh-handler)]
      (try
        (seed-accounts! h)
        (let [r (exec h "UPDATE t SET bal = bal + -30 WHERE id = 2")]
          (is (nil? (.error r)))
          (is (= "UPDATE 1" (tag r))))
        ;; Only row 2 changed.
        (is (= [["1" "100"] ["2" "170"] ["3" "300"]] (table-state h)))
        ;; Positive delta on another row, same statement family.
        (is (= "UPDATE 1" (tag (exec h "UPDATE t SET bal = bal + 5 WHERE id = 1"))))
        (is (= [["1" "105"] ["2" "170"] ["3" "300"]] (table-state h)))
        ;; Plain assignment (no arithmetic).
        (is (= "UPDATE 1" (tag (exec h "UPDATE t SET bal = 42 WHERE id = 3"))))
        (is (= [["1" "105"] ["2" "170"] ["3" "42"]] (table-state h)))
        (finally (release! h))))))

(deftest templated-delete
  (testing "DELETE FROM t WHERE id = N removes exactly that row"
    (is (some? (template/parameterize-numbers "DELETE FROM t WHERE id = 3")))
    (let [h (fresh-handler)]
      (try
        (seed-accounts! h)
        (let [r (exec h "DELETE FROM t WHERE id = 3")]
          (is (nil? (.error r)))
          (is (= "DELETE 1" (tag r))))
        (is (= [["1" "100"] ["2" "200"]] (table-state h)))
        ;; A second literal, same statement family (cached shape).
        (is (= "DELETE 1" (tag (exec h "DELETE FROM t WHERE id = 1"))))
        (is (= [["2" "200"]] (table-state h)))
        ;; No-match literal → DELETE 0, nothing else touched.
        (is (= "DELETE 0" (tag (exec h "DELETE FROM t WHERE id = 99"))))
        (is (= [["2" "200"]] (table-state h)))
        (finally (release! h))))))

(deftest templated-update-in-explicit-tx
  (testing "templated UPDATE inside BEGIN/COMMIT commits atomically"
    (let [h (fresh-handler)]
      (try
        (seed-accounts! h)
        (is (nil? (.error (exec h "BEGIN"))))
        (is (= "UPDATE 1" (tag (exec h "UPDATE t SET bal = bal + -30 WHERE id = 2"))))
        (is (= "DELETE 1" (tag (exec h "DELETE FROM t WHERE id = 3"))))
        ;; In-tx statements see the speculative state.
        (is (= [["1" "100"] ["2" "170"]] (table-state h)))
        (is (nil? (.error (exec h "COMMIT"))))
        (is (= [["1" "100"] ["2" "170"]] (table-state h)))
        (finally (release! h)))))
  (testing "templated UPDATE inside BEGIN/ROLLBACK leaves rows untouched"
    (let [h (fresh-handler)]
      (try
        (seed-accounts! h)
        (is (nil? (.error (exec h "BEGIN"))))
        (is (= "UPDATE 1" (tag (exec h "UPDATE t SET bal = bal + 1000 WHERE id = 1"))))
        (is (= "DELETE 1" (tag (exec h "DELETE FROM t WHERE id = 2"))))
        (is (nil? (.error (exec h "ROLLBACK"))))
        (is (= [["1" "100"] ["2" "200"] ["3" "300"]] (table-state h)))
        (finally (release! h))))))

(deftest templated-mixed-statements
  (testing "SELECT / UPDATE / DELETE interleaved, templated and not"
    (let [h (fresh-handler)]
      (try
        (seed-accounts! h)
        ;; Templated SELECT (WHERE literal) between the writes.
        (is (= [["200"]] (rows (exec h "SELECT bal FROM t WHERE id = 2"))))
        (is (= "UPDATE 1" (tag (exec h "UPDATE t SET bal = bal - 50 WHERE id = 2"))))
        (is (= [["150"]] (rows (exec h "SELECT bal FROM t WHERE id = 2"))))
        (is (= "DELETE 1" (tag (exec h "DELETE FROM t WHERE id = 1"))))
        ;; Untemplated full-table statements still see the right state.
        (is (= [["2" "150"] ["3" "300"]] (table-state h)))
        (is (= "UPDATE 2" (tag (exec h "UPDATE t SET bal = 0"))))
        (is (= [["2" "0"] ["3" "0"]] (table-state h)))
        (finally (release! h))))))

(deftest blacklisted-statements-keep-untemplated-behavior
  (testing "HAVING / BETWEEN / IN statements bypass the templater and still work"
    ;; Premise guard: these must NOT template (parse-time literal
    ;; consumers — see template/no-template-idents).
    (is (nil? (template/parameterize-numbers
               "SELECT id FROM t GROUP BY id HAVING sum(bal) > 150")))
    (is (nil? (template/parameterize-numbers
               "SELECT id FROM t WHERE bal BETWEEN 150 AND 250")))
    (is (nil? (template/parameterize-numbers
               "UPDATE t SET bal = 0 WHERE id IN (1, 3)")))
    (let [h (fresh-handler)]
      (try
        (seed-accounts! h)
        (is (= [["2"] ["3"]]
               (rows (exec h "SELECT id FROM t GROUP BY id HAVING sum(bal) > 150 ORDER BY id"))))
        (is (= [["2"]]
               (rows (exec h "SELECT id FROM t WHERE bal BETWEEN 150 AND 250"))))
        (is (= "UPDATE 2" (tag (exec h "UPDATE t SET bal = 0 WHERE id IN (1, 3)"))))
        (is (= [["1" "0"] ["2" "200"] ["3" "0"]] (table-state h)))
        (is (= "DELETE 2" (tag (exec h "DELETE FROM t WHERE id IN (1, 2)"))))
        (is (= [["3" "0"]] (table-state h)))
        (finally (release! h))))))

(deftest update-shape-stability-across-literals
  (testing "50 literals through one UPDATE statement family sum correctly"
    ;; Every iteration is the SAME templated shape ($1/$2), so parse
    ;; result, datalog parse and plan are all shared — correctness of
    ;; the final sum shows the shared shape re-binds values per call.
    (let [h (fresh-handler)]
      (try
        (is (nil? (.error (exec h "CREATE TABLE t(id INTEGER, bal INTEGER)"))))
        (is (nil? (.error (exec h "INSERT INTO t(id, bal) VALUES (1, 1000)"))))
        (is (nil? (.error (exec h "INSERT INTO t(id, bal) VALUES (2, 0)"))))
        (doseq [k (range 1 51)]
          (let [r (exec h (str "UPDATE t SET bal = bal + " k " WHERE id = 1"))]
            (is (nil? (.error r)) (str "delta " k ": " (.error r)))
            (is (= "UPDATE 1" (tag r)))))
        ;; 1000 + (1 + 2 + … + 50) = 2275; row 2 untouched.
        (is (= [["1" "2275"] ["2" "0"]] (table-state h)))
        ;; Same family with negative deltas drains it back down.
        (doseq [k (range 1 51)]
          (is (= "UPDATE 1" (tag (exec h (str "UPDATE t SET bal = bal + -" k " WHERE id = 1"))))))
        (is (= [["1" "1000"] ["2" "0"]] (table-state h)))
        (finally (release! h))))))
