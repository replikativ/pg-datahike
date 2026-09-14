(ns datahike.test.pg-row-constraint-metadata-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.core :as dc]
            [datahike.db :as db]
            [datahike.pg.constraints.row :as row]))

(defn- tx [db data] (:db-after (dc/with db data)))

(defn- declaration [attr type]
  {:db/ident attr :db/valueType type :db/cardinality :db.cardinality/one})

(defn- plain-db []
  (tx (db/empty-db nil {:schema-flexibility :write :keep-history? false})
      [(declaration :t/a :db.type/long)
       (declaration :t/b :db.type/string)
       (declaration :t/db-row-exists :db.type/boolean)
       (declaration :parent/id :db.type/long)
       (declaration :parent/db-row-exists :db.type/boolean)]))

(defn- metadata-schema []
  (vec
   (concat
    (map #(declaration % :db.type/string)
         [:pg/default-value :pg/default-arg
          :pg/check-name :pg/check-table :pg/check-expr
          :pg/fk-name :pg/fk-child-table :pg/fk-child-cols
          :pg/fk-parent-table :pg/fk-parent-cols
          :datahike.pg/domain-of :datahike.pg/enum-of
          :datahike.pg.domain/name :datahike.pg.domain/check-name
          :datahike.pg.domain/check-expr :datahike.pg.enum/name])
    [(declaration :pg/not-null :db.type/boolean)
     (declaration :pg/default-kind :db.type/keyword)
     (declaration :datahike.pg.domain/not-null :db.type/boolean)]
    (map #(assoc (declaration % :db.type/string) :db/cardinality :db.cardinality/many)
         [:datahike.pg.enum/values :datahike.pg.enum/unsafe-values]))))

(defn- constrained-db []
  (tx (tx (plain-db) (metadata-schema))
      [[:db/add [:db/ident :t/a] :pg/not-null true]
       [:db/add [:db/ident :t/a] :pg/default-kind :fn]
       [:db/add [:db/ident :t/a] :pg/default-value "now"]
       [:db/add [:db/ident :t/a] :datahike.pg/domain-of "positive"]
       [:db/add [:db/ident :t/b] :datahike.pg/enum-of "mood"]
       {:db/id 1000 :pg/check-name "positive_a" :pg/check-table "t" :pg/check-expr "a > 0"}
       {:db/id 1001 :pg/fk-name "parent_a" :pg/fk-child-table "t"
        :pg/fk-child-cols "[\"a\"]" :pg/fk-parent-table "parent"
        :pg/fk-parent-cols "[\"id\"]"}
       {:db/id 1002 :datahike.pg.domain/name "positive"
        :datahike.pg.domain/check-name "positive_value"
        :datahike.pg.domain/check-expr "VALUE > 0"
        :datahike.pg.domain/not-null true}
       {:db/id 1003 :datahike.pg.enum/name "mood"
        :datahike.pg.enum/values ["old" "new"]
        :datahike.pg.enum/unsafe-values ["new"]}]))

(deftest raw-metadata-is-complete-and-does-not-evaluate-defaults
  (let [db (constrained-db)
        metadata (with-redefs [row/eval-default (fn [& _] (throw (Exception. "default evaluated")))]
                   (row/constraint-metadata db "t"))
        columns (into {} (map (juxt :name identity)) (:columns metadata))]
    (is (= :t/a (get-in columns ["a" :attr])))
    (is (= [:fn "now" nil] (get-in columns ["a" :default])))
    (is (true? (get-in columns ["a" :not-null?])))
    (is (= [{:constraint "positive_a" :expression "a > 0"}] (:checks metadata)))
    (is (= [{:constraint "parent_a" :child-definition "[\"a\"]"
             :parent-definition "[\"id\"]" :child-attrs [:t/a]
             :parent-table "parent" :parent-attrs [:parent/id]}]
           (:fks metadata)))
    (is (= "VALUE > 0" (get-in metadata [:domain-enum "a" :check-expression])))
    (is (true? (get-in metadata [:domain-enum "a" :not-null?])))
    (is (= #{"old" "new"} (get-in metadata [:domain-enum "b" :values])))
    (is (= #{"new"} (get-in metadata [:domain-enum "b" :unsafe-values])))
    (let [plan (row/compile-constraint-metadata metadata)]
      (is (= (:columns metadata) (:columns plan)))
      (is (= [:t/a] (get-in plan [:fks 0 :child-attrs])))
      (is (not (contains? (first (:fks plan)) :child-definition)))
      (is (= "a > 0" (str (get-in plan [:checks 0 :ast]))))
      (is (some? (get-in plan [:domain-enum "a" :check-ast]))))))

(deftest malformed-sql-is-read-raw-and-fails-only-during-compilation
  (let [db (tx (constrained-db) [[:db/add 1000 :pg/check-expr "("]])
        metadata (row/constraint-metadata db "t")]
    (is (= "(" (get-in metadata [:checks 0 :expression])))
    (is (thrown? Exception (row/compile-constraint-metadata metadata)))))

(deftest raw-input-changes-are-visible-without-compiling-a-plan
  (let [db (constrained-db)
        original (row/constraint-metadata db "t")]
    (doseq [data [[[:db/add 1000 :pg/check-expr "a > 1"]]
                  [[:db/add 1001 :pg/fk-parent-cols "[\"missing\"]"]]
                  [[:db/add 1002 :datahike.pg.domain/check-expr "VALUE > 1"]]
                  [[:db/add 1002 :datahike.pg.domain/not-null false]]
                  [[:db/retract 1003 :datahike.pg.enum/unsafe-values "new"]]
                  [[:db/add [:db/ident :t/a] :pg/default-value "current_date"]]]]
      (is (not= original (row/constraint-metadata (tx db data) "t"))))
    (is (= original (row/constraint-metadata (tx db [{:t/a 1 :t/b "old"}]) "t")))))

(deftest empty-definitions-do-not-depend-on-metadata-schema-presence
  (let [db (plain-db)
        before (row/constraint-metadata db "t")
        after (row/constraint-metadata (tx db (metadata-schema)) "t")]
    (is (= before after))
    (is (= [] (:checks before)))
    (is (= [] (:fks before)))))
