(ns datahike.test.pg-arrays-test
  "Unit tests for the `datahike.pg.arrays` core value type — PgArray
   construction, subscripting, slicing, text codec round-trips, and
   array operations.

   These tests live below the SQL layer: they exercise the value type
   directly so failures localise to codec/semantics regressions without
   pulling in the translator. Integration with SQL (ARRAY[…] literals,
   fn results, subscript expressions, ANY/ALL) is covered in
   pg_array_sql_test.clj."
  (:require [clojure.test :refer [deftest testing is]]
            [datahike.pg.arrays :as arr]))

;; ---------------------------------------------------------------------------
;; Construction + predicates
;; ---------------------------------------------------------------------------

(deftest construct-empty
  (let [a (arr/array :int4 [])]
    (is (arr/array? a))
    (is (zero? (arr/length a)))
    (is (= :int4 (arr/element-type a)))))

(deftest construct-int8
  (let [a (arr/array :int8 [1 2 3])]
    (is (arr/array? a))
    (is (= 3 (arr/length a)))
    (is (= [1 2 3] (:elements a)))))

(deftest construct-text-with-nil
  (let [a (arr/array :text ["a" nil "c"])]
    (is (= 3 (arr/length a)))
    (is (nil? (nth (:elements a) 1)))))

(deftest array-predicate-rejects-non-arrays
  (is (not (arr/array? nil)))
  (is (not (arr/array? [1 2 3])))
  (is (not (arr/array? "not an array")))
  (is (not (arr/array? {}))))

(deftest equality-is-structural
  (is (= (arr/array :int8 [1 2 3]) (arr/array :int8 [1 2 3])))
  (is (not= (arr/array :int8 [1 2 3]) (arr/array :int4 [1 2 3])))
  (is (not= (arr/array :int8 [1 2 3]) (arr/array :int8 [1 2]))))

;; ---------------------------------------------------------------------------
;; Subscripting (1-indexed, nil on out-of-range per PG semantics)
;; ---------------------------------------------------------------------------

(deftest subscript-basic
  (let [a (arr/array :int8 [10 20 30])]
    (is (= 10 (arr/subscript a 1)))
    (is (= 20 (arr/subscript a 2)))
    (is (= 30 (arr/subscript a 3)))))

(deftest subscript-zero-returns-nil
  (testing "PG: arr[0] returns NULL (below lbound=1)"
    (is (nil? (arr/subscript (arr/array :int8 [10 20 30]) 0)))))

(deftest subscript-negative-returns-nil
  (is (nil? (arr/subscript (arr/array :int8 [10 20 30]) -1))))

(deftest subscript-out-of-range-returns-nil
  (testing "PG: arr[N] for N > length returns NULL (not error)"
    (is (nil? (arr/subscript (arr/array :int8 [10 20 30]) 4)))
    (is (nil? (arr/subscript (arr/array :int8 [10 20 30]) 1000)))))

(deftest subscript-on-empty-returns-nil
  (is (nil? (arr/subscript (arr/array :text []) 1))))

(deftest subscript-nil-index-returns-nil
  (testing "SQL NULL index propagates through subscript"
    (is (nil? (arr/subscript (arr/array :int8 [10 20 30]) nil)))))

(deftest subscript-returns-nil-for-nil-element
  (let [a (arr/array :text ["a" nil "c"])]
    (is (nil? (arr/subscript a 2)))))

;; ---------------------------------------------------------------------------
;; Slicing (1-indexed, clamped)
;; ---------------------------------------------------------------------------

(deftest slice-basic
  (let [a (arr/array :int8 [10 20 30 40 50])]
    (is (= [20 30 40] (:elements (arr/slice a 2 4))))))

(deftest slice-clamps-upper
  (let [a (arr/array :int8 [10 20 30])]
    (is (= [20 30] (:elements (arr/slice a 2 10))))))

(deftest slice-clamps-lower
  (let [a (arr/array :int8 [10 20 30])]
    (is (= [10 20] (:elements (arr/slice a -5 2))))))

(deftest slice-empty-when-reversed
  (let [a (arr/array :int8 [10 20 30])]
    (is (= [] (:elements (arr/slice a 3 1))))))

(deftest slice-preserves-element-type
  (let [a (arr/array :text ["a" "b" "c" "d"])]
    (is (= :text (arr/element-type (arr/slice a 1 2))))))

;; ---------------------------------------------------------------------------
;; Membership + operators (ANY / ALL / containment / overlap / concat)
;; ---------------------------------------------------------------------------

(deftest member?-basic
  (let [a (arr/array :int8 [10 20 30])]
    (is (arr/member? a 20))
    (is (not (arr/member? a 99)))))

(deftest member?-handles-nil
  (testing "PG three-valued: a = ANY(arr) with NULL never returns true but doesn't error"
    (is (nil? (arr/member? (arr/array :int8 [nil 2 3]) 1)))
    (is (arr/member? (arr/array :int8 [nil 2 3]) 2))))

(deftest any-match-predicate
  (let [a (arr/array :int8 [10 20 30])]
    (is (arr/any-match? a #(= 20 %)))
    (is (not (arr/any-match? a #(= 99 %))))))

(deftest all-match-predicate
  (let [a (arr/array :int8 [10 20 30])]
    (is (arr/all-match? a #(< % 100)))
    (is (not (arr/all-match? a #(< % 25))))))

(deftest contains?-subset
  (let [a (arr/array :int8 [1 2 3 4 5])
        b (arr/array :int8 [2 4])
        c (arr/array :int8 [2 99])]
    (is (arr/contains-arr? a b))
    (is (not (arr/contains-arr? a c)))))

(deftest overlap?-intersection
  (let [a (arr/array :int8 [1 2 3])
        b (arr/array :int8 [3 4 5])
        c (arr/array :int8 [7 8 9])]
    (is (arr/overlap? a b))
    (is (not (arr/overlap? a c)))))

(deftest concat-arrays
  (let [a (arr/array :int8 [1 2])
        b (arr/array :int8 [3 4])
        r (arr/concat-arrs a b)]
    (is (= [1 2 3 4] (:elements r)))
    (is (= :int8 (arr/element-type r)))))

;; ---------------------------------------------------------------------------
;; Text codec — to-pg-text / from-pg-text / round-trip
;; ---------------------------------------------------------------------------

(deftest text-format-empty
  (is (= "{}" (arr/to-pg-text (arr/array :int4 [])))))

(deftest text-format-int
  (is (= "{1,2,3}" (arr/to-pg-text (arr/array :int8 [1 2 3])))))

(deftest text-format-unquoted-strings
  (testing "Strings with no special chars emit unquoted"
    (is (= "{alice,bob,carol}" (arr/to-pg-text (arr/array :text ["alice" "bob" "carol"]))))))

(deftest text-format-quotes-when-needed
  (testing "PG quotes elements with , or \" or \\ or whitespace or {}"
    (is (= "{\"a,b\",c}" (arr/to-pg-text (arr/array :text ["a,b" "c"]))))
    (is (= "{\"has \\\"quote\\\"\"}" (arr/to-pg-text (arr/array :text ["has \"quote\""]))))
    (is (= "{\"with space\"}" (arr/to-pg-text (arr/array :text ["with space"]))))))

(deftest text-format-null-element
  (testing "NULL elements emit as unquoted NULL token"
    (is (= "{a,NULL,c}" (arr/to-pg-text (arr/array :text ["a" nil "c"]))))
    (is (= "{NULL}" (arr/to-pg-text (arr/array :text [nil]))))))

(deftest text-format-empty-string-quoted
  (testing "Empty-string element must be quoted to distinguish from NULL"
    (is (= "{\"\"}" (arr/to-pg-text (arr/array :text [""]))))))

(deftest text-format-bool
  (is (= "{t,f,t}" (arr/to-pg-text (arr/array :bool [true false true])))))

(deftest text-format-float
  (is (= "{1.5,2.5}" (arr/to-pg-text (arr/array :float8 [1.5 2.5])))))

(deftest text-format-nested-pg-array
  (testing "Multi-dim ARRAY[[1,0],[0,1]] renders as PG canonical {{1,0},{0,1}}"
    (let [inner1 (arr/array :int8 [1 0])
          inner2 (arr/array :int8 [0 1])
          outer  (arr/array :int8 [inner1 inner2])]
      (is (= "{{1,0},{0,1}}" (arr/to-pg-text outer)))))
  (testing "Nested clojure vector — same canonical form"
    (is (= "{{1,0},{0,1}}" (arr/to-pg-text (arr/array :int8 [[1 0] [0 1]])))))
  (testing "3D arrays compose"
    (is (= "{{{1,2},{3,4}},{{5,6},{7,8}}}"
           (arr/to-pg-text (arr/array :int8 [[[1 2] [3 4]] [[5 6] [7 8]]]))))))

(deftest text-parse-empty
  (is (= [] (:elements (arr/from-pg-text "{}" :int4)))))

(deftest text-parse-int
  (is (= [1 2 3] (:elements (arr/from-pg-text "{1,2,3}" :int8)))))

(deftest text-parse-text-unquoted
  (is (= ["alice" "bob"] (:elements (arr/from-pg-text "{alice,bob}" :text)))))

(deftest text-parse-text-quoted-with-comma
  (is (= ["a,b" "c"] (:elements (arr/from-pg-text "{\"a,b\",c}" :text)))))

(deftest text-parse-null-token
  (is (= ["a" nil "c"] (:elements (arr/from-pg-text "{a,NULL,c}" :text))))
  (testing "NULL is case-insensitive per PG"
    (is (= [nil nil] (:elements (arr/from-pg-text "{null,Null}" :text))))))

(deftest text-parse-empty-string-quoted
  (is (= [""] (:elements (arr/from-pg-text "{\"\"}" :text)))))

(deftest text-parse-escaped-chars
  (is (= ["has \"quote\""] (:elements (arr/from-pg-text "{\"has \\\"quote\\\"\"}" :text)))))

(deftest text-round-trip-text
  (testing "to→from idempotence for :text element type"
    (doseq [elts [[] ["a" "b"] ["a,b" "c"] ["a" nil "b"] [""]
                  ["" "x"] ["\"quoted\""] ["line1\nline2"]]]
      (let [a (arr/array :text elts)
            r (arr/from-pg-text (arr/to-pg-text a) :text)]
        (is (= elts (:elements r))
            (str "round-trip failed for: " (pr-str elts)))))))

(deftest text-round-trip-int
  (testing "to→from idempotence for :int8 element type"
    (doseq [elts [[] [1 2 3] [42] [0 -1 1000000000]]]
      (let [a (arr/array :int8 elts)
            r (arr/from-pg-text (arr/to-pg-text a) :int8)]
        (is (= elts (:elements r))
            (str "round-trip failed for: " (pr-str elts)))))))

(deftest text-round-trip-bool
  (testing "to→from idempotence for :bool element type"
    (doseq [elts [[] [true] [false] [true false true] [true nil false]]]
      (let [a (arr/array :bool elts)
            r (arr/from-pg-text (arr/to-pg-text a) :bool)]
        (is (= elts (:elements r))
            (str "round-trip failed for: " (pr-str elts)))))))
