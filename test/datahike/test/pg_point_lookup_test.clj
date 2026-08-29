(ns datahike.test.pg-point-lookup-test
  "Prepared equality on a unique column must remain an indexable point seek.

   A query-time `seek-key` function made the pattern value look unbound to
   Datahike's direct executor. The replacement coerces each parameter once at
   Bind/Execute and supplies the storage-compatible key as a scalar `:in`
   binding. These tests pin both the lowering shape and the cases where SQL
   equality must match no row rather than accidentally scanning or rounding."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.fns :as fns]
            [datahike.pg.sql.params :as params]
            [datahike.pg.types :as types]))

(defn- with-handler [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}
        _ (d/create-database cfg)
        conn (d/connect cfg)
        handler (pg/make-query-handler conn {})]
    (try
      (is (nil? (.error
                 (.execute handler
                           (str "CREATE TABLE point_lookup ("
                                "id bigint PRIMARY KEY, balance int, f float8 UNIQUE)")))))
      (is (nil? (.error
                 (.execute handler
                           (str "INSERT INTO point_lookup VALUES "
                                "(1, 100, 1.0), (2, 100, 2.0), (3, 300, 3.0), "
                                "(9007199254740992, 992, 4.0), "
                                "(9007199254740993, 993, 5.0), "
                                "(9223372036854775807, 701, 6.0)")))))
      (f handler)
      (finally
        (try (.close handler) (catch Exception _))
        (d/release conn)
        (d/delete-database cfg)))))

(defn- prepared-rows [handler parsed value]
  (let [bound (object-array 2)]
    (aset bound 1 value)
    (let [result (.executePrepared handler parsed bound)]
      (is (nil? (.error result)) (str (.error result)))
      (mapv vec (.rows result)))))

(deftest prepared-unique-equality-is-a-direct-seek-shape
  (with-handler
    (fn [handler]
      (let [parsed (.parse handler
                           "SELECT balance FROM point_lookup WHERE id = $1"
                           (int-array [1700]))
            query (:query parsed)
            transformed (filter #(get % ::params/coercion) (:in-args parsed))]
        (testing "the AVET value is a scalar input, not a derived query value"
          (is (= [[:seek-key :db.type/long]]
                 (mapv #(get % ::params/coercion) transformed)))
          (is (not-any? #{'datahike.pg.sql/seek-key}
                        (tree-seq coll? seq query))
              (pr-str query)))
        (testing "at-most-one cardinality needs neither sorting nor bag keys"
          (is (nil? (:order-by query)) (pr-str query))
          (is (nil? (:with query)) (pr-str query)))
        (testing "numeric parameters narrow exactly to integer storage"
          (is (= [["100"]] (prepared-rows handler parsed (bigdec "1.0"))))
          (is (= [["300"]] (prepared-rows handler parsed (bigdec "3"))))
          (is (= [] (prepared-rows handler parsed (bigdec "1.5")))
              "a fractional numeric must not round to an integer key")
          (is (= [] (prepared-rows handler parsed
                                   (bigdec "1.0000000000000000000000000001")))
              "integrality cannot be decided through a lossy double")
          (is (= [] (prepared-rows handler parsed
                                   (bigdec "9223372036854775808")))
              "positive overflow must not saturate to Long/MAX_VALUE")
          (is (= [] (prepared-rows handler parsed
                                   (bigdec "-9223372036854775809")))
              "negative overflow must not saturate to Long/MIN_VALUE")
          (is (= [["993"]]
                 (prepared-rows handler parsed (bigdec "9007199254740993")))
              "integers above 2^53 must retain every bit"))
        (testing "NULL uses a non-matching seek sentinel, never a nil wildcard"
          (is (= [] (prepared-rows handler parsed nil))))))))

(deftest each-storage-type-gets-its-own-coerced-input
  (with-handler
    (fn [handler]
      (let [parsed (.parse handler
                           (str "SELECT id FROM point_lookup "
                                "WHERE id = $1 AND f = $1")
                           (int-array [1700]))
            coercions (keep #(get % ::params/coercion) (:in-args parsed))]
        (is (= [[:seek-key :db.type/long]
                [:seek-key :db.type/double]]
               coercions))
        (is (= [["2"]] (prepared-rows handler parsed (bigdec "2.0"))))
        (is (= [] (prepared-rows handler parsed (bigdec "2.5"))))))))

(deftest float-to-exact-comparison-stays-on-the-predicate-path
  (with-handler
    (fn [handler]
      (let [parsed (.parse handler
                           "SELECT balance FROM point_lookup WHERE id = $1"
                           (int-array [701]))]
        (is (empty? (keep #(get % ::params/coercion) (:in-args parsed)))
            (pr-str (:query parsed)))
        (is (= [["992"] ["993"]]
               (prepared-rows handler parsed (double 9007199254740992)))
            "PostgreSQL compares bigint with float8 in lossy float space")))))

(deftest prepared-numeric-specials-use-their-storage-keys
  (with-handler
    (fn [handler]
      (is (nil? (.error (.execute handler
                                  "CREATE TABLE numeric_point(id int PRIMARY KEY, v numeric UNIQUE)"))))
      (is (nil? (.error (.execute handler
                                  (str "INSERT INTO numeric_point VALUES "
                                       "(1, 'NaN'::numeric), "
                                       "(2, 'Infinity'::numeric), "
                                       "(3, '-Infinity'::numeric)")))))
      (let [parsed (.parse handler
                           "SELECT id FROM numeric_point WHERE v = $1"
                           (int-array [1700]))]
        (is (= [["1"]] (prepared-rows handler parsed types/nan-numeric)))
        (is (= [["2"]] (prepared-rows handler parsed types/inf-numeric)))
        (is (= [["3"]] (prepared-rows handler parsed types/-inf-numeric)))))))

(deftest null-seek-cannot-collide-with-a-stored-keyword
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}
        _ (d/create-database cfg)
        conn (d/connect cfg)
        handler (pg/make-query-handler conn {})]
    (try
      (d/transact conn
                  [{:db/ident :kw/id
                    :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one
                    :db/unique :db.unique/identity}
                   {:db/ident :kw/state
                    :db/valueType :db.type/keyword
                    :db/cardinality :db.cardinality/one
                    :db/unique :db.unique/value}])
      (d/transact conn [{:kw/id "collision" :kw/state fns/seek-no-match}])
      (let [parsed (.parse handler
                           "SELECT id FROM kw WHERE state = $1"
                           (int-array [25]))]
        (is (= [] (prepared-rows handler parsed nil))
            "NULL must not equal a user-stored value that resembles a sentinel")
        (is (= [["collision"]]
               (prepared-rows handler parsed "datahike.pg.sql.fns/seek-no-match"))
            "prepared text input is coerced to native keyword storage"))
      (finally
        (try (.close handler) (catch Exception _))
        (d/release conn)
        (d/delete-database cfg)))))

(deftest non-unique-equality-keeps-sql-bag-semantics
  (with-handler
    (fn [handler]
      (let [parsed (.parse handler
                           "SELECT balance FROM point_lookup WHERE balance = $1"
                           (int-array [23]))]
        (is (or (seq (get-in parsed [:query :order-by]))
                (seq (get-in parsed [:query :with])))
            (pr-str (:query parsed)))
        (is (= [["100"] ["100"]] (prepared-rows handler parsed (long 100))))))))

(deftest unique-cardinality-proof-stays-within-its-sound-boundary
  (with-handler
    (fn [handler]
      (doseq [sql ["SELECT balance FROM point_lookup WHERE id = $1 OR balance = 300"
                   "SELECT balance FROM point_lookup WHERE NOT (id = $1)"]]
        (let [parsed (.parse handler sql (int-array [20]))]
          (is (empty? (keep #(get % ::params/coercion) (:in-args parsed)))
              (str "disjunctive/negated equality became a ground pattern: " sql))
          (is (or (seq (get-in parsed [:query :order-by]))
                  (seq (get-in parsed [:query :with])))
              (str "disjunctive/negated equality claimed <=1 row: " sql))))
      (let [parsed (.parse handler
                           (str "SELECT p2.id FROM point_lookup p "
                                "JOIN point_lookup p2 ON p2.balance = p.balance "
                                "WHERE p.id = $1")
                           (int-array [20]))]
        (is (seq (keep #(get % ::params/coercion) (:in-args parsed)))
            "the unique predicate itself remains seekable")
        (is (or (seq (get-in parsed [:query :order-by]))
                (seq (get-in parsed [:query :with])))
            "a unique row can still fan out through a join")
        (is (= #{["1"] ["2"]}
               (set (prepared-rows handler parsed (long 1)))))))))
