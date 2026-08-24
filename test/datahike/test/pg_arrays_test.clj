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

(deftest text-parse-item-whitespace
  (testing "whitespace outside an item is ignored, including around quotes"
    (is (= ["a" "b" " c "]
           (:elements (arr/from-pg-text "{  a  ,   \"b\"  ,\" c \" }" :text))))))

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

;; ---------------------------------------------------------------------------
;; Multi-dimensional arrays — Phase A
;; ---------------------------------------------------------------------------

(deftest construct-2d
  (testing "2-D array carries explicit dims/lbounds"
    (let [a (arr/array :int8 [[1 2] [3 4]])]
      (is (= 2 (arr/ndim a)))
      (is (= [2 2] (:dims a)))
      (is (= [1 1] (:lbounds a)))
      (is (= 2 (arr/length a))                    "outer dim")
      (is (= 2 (arr/length-d a 1))                "dim 1 size")
      (is (= 2 (arr/length-d a 2))                "dim 2 size")
      (is (= 1 (arr/lbound a 1)))
      (is (= 1 (arr/lbound a 2)))
      (is (= 2 (arr/ubound a 1)))
      (is (= 2 (arr/ubound a 2))))))

(deftest construct-2d-rectangular
  (testing "2x3 array — non-square dims"
    (let [a (arr/array :int8 [[1 2 3] [4 5 6]])]
      (is (= [2 3] (:dims a)))
      (is (= 2 (arr/length-d a 1)))
      (is (= 3 (arr/length-d a 2))))))

(deftest construct-3d
  (testing "3-D array dims walk all levels"
    (let [a (arr/array :int8 [[[1 2] [3 4]] [[5 6] [7 8]]])]
      (is (= 3 (arr/ndim a)))
      (is (= [2 2 2] (:dims a))))))

(deftest construct-rejects-ragged
  (testing "PG rejects ragged arrays — sub-arrays must agree in shape"
    (is (thrown? Exception
                 (arr/array :int8 [[1 2] [3]])))))

(deftest length-d-out-of-range
  (testing "length-d returns nil for out-of-range dim (caller maps to NULL)"
    (let [a (arr/array :int8 [[1 2] [3 4]])]
      (is (nil? (arr/length-d a 0)))
      (is (nil? (arr/length-d a 3)))
      (is (= 2 (arr/length-d a 1)))
      (is (= 2 (arr/length-d a 2))))))

(deftest subscript-2d-rank-reduces
  (testing "subscript on 2-D returns 1-D row preserving lbounds"
    (let [a (arr/array :int8 [[1 2] [3 4]])
          row (arr/subscript a 1)]
      (is (arr/array? row))
      (is (= 1 (arr/ndim row)))
      (is (= [2] (:dims row)))
      (is (= [1 2] (:elements row))))))

(deftest subscript-2d-then-1d
  (testing "Two subscripts compose to scalar"
    (let [a (arr/array :int8 [[10 20] [30 40]])]
      (is (= 30 (arr/subscript (arr/subscript a 2) 1)))
      (is (= 40 (arr/subscript (arr/subscript a 2) 2))))))

(deftest slice-2d-preserves-inner-shape
  (testing "slice over outer dim keeps inner shape and lbounds"
    (let [a (arr/array :int8 [[1 2] [3 4] [5 6]])
          sl (arr/slice a 2 3)]
      (is (= 2 (arr/ndim sl)))
      (is (= [2 2] (:dims sl)))
      (is (= [[3 4] [5 6]] (:elements sl))))))

(deftest text-format-multidim-2d
  (testing "to-pg-text emits canonical {{…},{…}} for 2-D"
    (is (= "{{1,2},{3,4}}"
           (arr/to-pg-text (arr/array :int8 [[1 2] [3 4]]))))))

(deftest text-format-multidim-3d
  (testing "to-pg-text emits canonical {{{…},{…}},…} for 3-D"
    (is (= "{{{1,2},{3,4}},{{5,6},{7,8}}}"
           (arr/to-pg-text (arr/array :int8 [[[1 2] [3 4]] [[5 6] [7 8]]]))))))

(deftest text-parse-multidim-2d
  (testing "from-pg-text parses nested braces into nested elements"
    (let [a (arr/from-pg-text "{{1,2},{3,4}}" :int8)]
      (is (= [[1 2] [3 4]] (:elements a)))
      (is (= [2 2] (:dims a))))))

(deftest text-parse-multidim-with-nulls
  (testing "NULL elements are preserved at any depth"
    (let [a (arr/from-pg-text "{{1,NULL},{NULL,4}}" :int8)]
      (is (= [[1 nil] [nil 4]] (:elements a))))))

(deftest text-parse-rejects-ragged
  (testing "Ragged literal raises"
    (is (thrown-with-msg? Exception #"ragged"
                          (arr/from-pg-text "{{1,2},{3,4,5}}" :int8)))))

(deftest text-parse-lbound-prefix
  (testing "Non-default lbound is parsed and preserved"
    (let [a (arr/from-pg-text "[2:4]={a,b,c}" :text)]
      (is (= [2] (:lbounds a)))
      (is (= ["a" "b" "c"] (:elements a)))
      (is (= 2 (arr/lbound a 1)))
      (is (= 4 (arr/ubound a 1))))))

(deftest text-parse-lbound-prefix-2d
  (testing "Multi-dim lbound prefix"
    (let [a (arr/from-pg-text "[1:2][1:2]={{1,0},{0,1}}" :int8)]
      (is (= [1 1] (:lbounds a)))
      (is (= [2 2] (:dims a))))))

(deftest text-emit-lbound-prefix
  (testing "to-pg-text emits [lo:hi]= prefix only when lbound != 1"
    (let [a (arr/array :text ["a" "b" "c"] [3] [2])]
      (is (= "[2:4]={a,b,c}" (arr/to-pg-text a))))
    (let [b (arr/array :text ["a" "b" "c"])]
      (is (= "{a,b,c}" (arr/to-pg-text b)) "default lbound omits prefix"))))

(deftest text-round-trip-multidim
  (testing "to→from→to round-trip preserves shape and elements"
    (doseq [shapes [[[1 2] [3 4]]
                    [[1 2 3] [4 5 6]]
                    [[[1 2] [3 4]] [[5 6] [7 8]]]
                    [["a" "b"] ["c" "d"]]
                    [[true false] [false true]]
                    [[1 nil 3] [nil 5 nil]]]]
      (let [a (arr/array (cond (string? (-> shapes flatten first)) :text
                               (boolean? (-> shapes flatten first)) :bool
                               :else :int8)
                         shapes)
            r (arr/from-pg-text (arr/to-pg-text a) (:elem-type a))]
        (is (= a r) (str "round-trip failed for: " (pr-str shapes)))))))

(deftest flat-elements-walks-all-levels
  (testing "flat-elements yields leaf values regardless of depth"
    (is (= [1 2 3 4]
           (arr/flat-elements (arr/array :int8 [[1 2] [3 4]]))))
    (is (= [1 2 3 4 5 6 7 8]
           (arr/flat-elements (arr/array :int8 [[[1 2] [3 4]] [[5 6] [7 8]]]))))))

(deftest member-walks-multidim
  (testing "x = ANY(arr) hits leaves at any depth"
    (let [a (arr/array :int8 [[1 2] [3 4]])]
      (is (true? (arr/member? a 3)))
      (is (false? (arr/member? a 99))))))

(deftest contains-arr-walks-multidim
  (testing "@> works on flattened leaves"
    (let [a (arr/array :int8 [[1 2 3] [4 5 6]])
          b (arr/array :int8 [[2 5]])]
      (is (true? (arr/contains-arr? a b))))))

;; ---------------------------------------------------------------------------
;; Type registry — element-keyword resolution
;; ---------------------------------------------------------------------------

(deftest registry-parse-array-type-name
  (testing "parse-array-type-name recognises common forms"
    (is (= {:elem :int4 :pg-name "_int4" :ndim 1}
           (datahike.pg.types/parse-array-type-name "int[]")))
    (is (= {:elem :int4 :pg-name "_int4" :ndim 1}
           (datahike.pg.types/parse-array-type-name "integer[]")))
    (is (= {:elem :text :pg-name "_text" :ndim 2}
           (datahike.pg.types/parse-array-type-name "text[][]")))
    (is (= {:elem :int4 :pg-name "_int4" :ndim 1}
           (datahike.pg.types/parse-array-type-name "int ARRAY")))
    (is (= {:elem :int4 :pg-name "_int4" :ndim 1}
           (datahike.pg.types/parse-array-type-name "int ARRAY[3]"))
        "ARRAY[N] size annotation is informational")
    (is (= {:elem :numeric :pg-name "_numeric" :ndim 1}
           (datahike.pg.types/parse-array-type-name "numeric(10,2)[]"))
        "(p,s) typmod stripped")
    (is (= {:elem :varchar :pg-name "_varchar" :ndim 1}
           (datahike.pg.types/parse-array-type-name "varchar(255)[]")))
    (is (nil? (datahike.pg.types/parse-array-type-name "int")) "scalar")
    (is (nil? (datahike.pg.types/parse-array-type-name "")) "empty")
    (is (nil? (datahike.pg.types/parse-array-type-name "unknown_type[]")))))

(deftest registry-pg-name-array-oids
  (testing "Every scalar's _T entry resolves to its array OID"
    (is (= 1007 (get datahike.pg.types/pg-name->oid "_int4")))
    (is (= 1009 (get datahike.pg.types/pg-name->oid "_text")))
    (is (= 1016 (get datahike.pg.types/pg-name->oid "_int8")))
    (is (= 1231 (get datahike.pg.types/pg-name->oid "_numeric")))
    (is (= 1000 (get datahike.pg.types/pg-name->oid "_bool")))))

;; ---------------------------------------------------------------------------
;; Phase C: PG-format binary array codec — direct Java codec round-trip.
;; Exercises encodeArrayBinary / decodeArrayBinary without pgjdbc so the
;; bytes themselves are verified.
;; ---------------------------------------------------------------------------

(defn- encode-arr [oid text]
  (datahike.pg.PgParamCodec/encodeArrayBinary (int oid) text))

(defn- decode-arr [oid bytes]
  (datahike.pg.PgParamCodec/decodeArrayBinary (int oid) bytes))

(deftest binary-1d-int-roundtrip
  (let [oid 1007  ;; _int4
        bytes (encode-arr oid "{10,20,30}")]
    (is (some? bytes))
    (is (= "{10,20,30}" (decode-arr oid bytes)))))

(deftest binary-empty-roundtrip
  (let [oid 1007]
    (is (= "{}" (decode-arr oid (encode-arr oid "{}"))))))

(deftest binary-1d-with-nulls
  (let [oid 1007]
    (is (= "{1,NULL,3}" (decode-arr oid (encode-arr oid "{1,NULL,3}"))))))

(deftest binary-text-roundtrip
  (let [oid 1009]  ;; _text
    (is (= "{alice,bob}" (decode-arr oid (encode-arr oid "{alice,bob}"))))))

(deftest binary-2d-int-roundtrip
  (let [oid 1007
        bytes (encode-arr oid "{{1,2},{3,4}}")
        decoded (decode-arr oid bytes)]
    (is (some? bytes))
    (is (= "{{1,2},{3,4}}" decoded))))

(deftest binary-text-with-special-chars
  (let [oid 1009
        ;; PG quotes elements containing comma; round-trip preserves the
        ;; quote semantics — value comes back equivalent in text form.
        original "{\"has,comma\",plain}"]
    (is (= original (decode-arr oid (encode-arr oid original))))))

(deftest binary-bool-roundtrip
  (let [oid 1000  ;; _bool
        bytes (encode-arr oid "{t,f,t}")]
    (is (= "{t,f,t}" (decode-arr oid bytes)))))

(deftest binary-non-default-lbound
  (testing "Non-default lbound prefix round-trips through binary"
    (let [oid 1007
          original "[2:4]={1,2,3}"]
      (is (= original (decode-arr oid (encode-arr oid original)))))))
