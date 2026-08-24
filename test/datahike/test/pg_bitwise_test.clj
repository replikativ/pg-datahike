(ns datahike.test.pg-bitwise-test
  "Bitwise operators — `&` `|` `~` `<<` `>>` — plus `^`, over integers and
   bit strings.

   `~` was the dangerous one: JSqlParser parses it as a SignedExpression,
   which fell through to the identity branch, so `SELECT ~1` answered
   **1**. Not an unsupported-feature error — a silent wrong answer. The
   rest (`&` `|` `<<` `>>` `^`) all raised `0A000 … is not supported`.

   Two precedence facts drive the expectations, both taken from PG's
   grammar (gram.y's precedence declarations) and confirmed against a
   live server:

     1. `&` `|` `<<` `>>` `||` and prefix `~` all share ONE level, and it
        sits BELOW `+`/`-`. So `4 | 3 & 1` is `(4|3) & 1` = 1 — NOT C's
        `4 | (3&1)` = 5 — and `1 << 2 + 3` is `1 << 5` = 32.
     2. `^` is EXPONENTIATION, not xor (PG spells xor `#`). It binds
        tighter than `*` and is LEFT-associative, so `2^3^3` = 512 and
        `-2 ^ 2` = 4.

   JSqlParser's own precedence happens to match PG's for every binary
   case above. It does NOT match for prefix `~`, which it binds to its
   operand: PG's `~1 + 1` is `~(1+1)` = -3 where JSqlParser's tree says
   `(~1)+1` = -1. translate-binary-arith re-associates for exactly the
   higher-precedence operators, and deliberately leaves the same-level
   ones alone (`~1 & 3` is `(~1) & 3` = 2 in PG too).

   Every expectation was captured from PostgreSQL 17 by differential
   testing."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.fns :as fns])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def oid-int8 20)
(def oid-int4 23)
(def oid-float8 701)
(def oid-bit 1560)

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*handler* (pg/make-query-handler conn)]
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- v
  "The single scalar a one-row/one-column SELECT produced, as wire text."
  [sql]
  (ffirst (mapv vec (.-rows ^PgWireServer$QueryResult (.execute *handler* sql)))))

(defn- oids [sql]
  (vec (.-columnOids ^PgWireServer$QueryResult (.execute *handler* sql))))

(defn- err [sql]
  (try (.-error ^PgWireServer$QueryResult (.execute *handler* sql))
       (catch Exception e (ex-message e))))

;; ---------------------------------------------------------------------------
;; Integers
;; ---------------------------------------------------------------------------

(deftest integer-bitwise-basics
  (is (= "1" (v "SELECT 5 & 3")))
  (is (= "7" (v "SELECT 5 | 3")))
  (is (= "20" (v "SELECT 5 << 2")))
  (is (= "1" (v "SELECT 5 >> 2")))
  (is (= "255" (v "SELECT (-1) & 255"))))

(deftest bitwise-operators-are-strict
  (testing "every supported SQL bitwise operator returns NULL if any input is NULL"
    (doseq [sql ["SELECT 42 & NULL"
                 "SELECT NULL | 42"
                 "SELECT ~NULL"
                 "SELECT 42 << NULL"
                 "SELECT NULL >> 2"]]
      (is (nil? (v sql)) sql)))
  (testing "the xor implementation is strict even while SQL # remains unsupported"
    (is (= :__null__ (fns/sql-bit-xor 42 :__null__)))))

(deftest bitwise-not-is-not-the-identity
  (testing "SELECT ~1 answered 1 — a silent wrong answer, not an error"
    (is (= "-2" (v "SELECT ~1")))
    (is (= "-6" (v "SELECT ~5")))
    (is (= "0" (v "SELECT ~(-1)")))))

(deftest right-shift-is-arithmetic
  (testing "PG emits a bare C >> on a signed operand, so the sign propagates"
    (is (= "-4" (v "SELECT -8 >> 1")))
    (is (= "-1" (v "SELECT -1 >> 5")))))

;; ---------------------------------------------------------------------------
;; Precedence — the whole reason this needs testing
;; ---------------------------------------------------------------------------

(deftest bitwise-operators-share-one-level-below-arithmetic
  (testing "`&` and `|` are equal precedence and left-associative, so this
            is (4|3)&1 = 1, not C's 4|(3&1) = 5"
    (is (= "1" (v "SELECT 4 | 3 & 1"))))
  (testing "and both bind looser than + -"
    (is (= "32" (v "SELECT 1 << 2 + 3")) "1 << (2+3), not (1<<2)+3 = 7")
    (is (= "0" (v "SELECT 4 & 2 + 1")) "4 & (2+1), not (4&2)+1 = 1")))

(deftest prefix-not-binds-looser-than-arithmetic
  (testing "PG's `~` is at the generic-operator level, so it swallows the
            higher-precedence operator on its right"
    (is (= "-3" (v "SELECT ~1 + 1")) "~(1+1), not (~1)+1 = -1")
    (is (= "-7" (v "SELECT ~2 * 3")) "~(2*3), not (~2)*3 = -9"))
  (testing "but NOT the operators at its own level, which are left-assoc"
    (is (= "2" (v "SELECT ~1 & 3")) "(~1) & 3")
    (is (= "-2" (v "SELECT ~1 | 8")) "(~1) | 8")
    (is (= "-4" (v "SELECT ~1 << 1")) "(~1) << 1"))
  (testing "explicit parens agree with the re-associated reading"
    (is (= (v "SELECT ~(1+1)") (v "SELECT ~1 + 1")))
    (is (= "-1" (v "SELECT (~1) + 1")))))

;; ---------------------------------------------------------------------------
;; `^` is exponentiation
;; ---------------------------------------------------------------------------

(deftest caret-is-exponentiation-not-xor
  (testing "PG spells xor `#`; `^` is power"
    (is (= "125" (v "SELECT 5 ^ 3")) "5^3, not 5 xor 3 = 6")
    (is (= "1024" (v "SELECT 2 ^ 10"))))
  (testing "left-associative, unlike the mathematical convention"
    (is (= "512" (v "SELECT 2 ^ 3 ^ 3")) "(2^3)^3, not 2^(3^3)"))
  (testing "binds tighter than *, looser than unary minus"
    (is (= "16" (v "SELECT 2 ^ 3 * 2")) "(2^3)*2")
    (is (= "4" (v "SELECT -2 ^ 2")) "(-2)^2, not -(2^2)"))
  (testing "and returns float8"
    (is (= [oid-float8] (oids "SELECT 5 ^ 3")))))

;; ---------------------------------------------------------------------------
;; Bit strings
;; ---------------------------------------------------------------------------

(deftest bit-string-bitwise
  (is (= "1000" (v "SELECT B'1100' & B'1010'")))
  (is (= "1110" (v "SELECT B'1100' | B'1010'")))
  (is (= "0011" (v "SELECT ~B'1100'")))
  (testing "the result is bit, never varbit — PG declares these only on bit"
    (is (= [oid-bit] (oids "SELECT B'1100' & B'1010'")))
    (is (= [oid-bit] (oids "SELECT ~B'1100'")))))

(deftest bit-string-shifts-preserve-width
  (testing "shift happens WITHIN the width, zero-filled — not a widening"
    (is (= "1000" (v "SELECT B'1100' << 1")) "not 11000")
    (is (= "0110" (v "SELECT B'1100' >> 1"))))
  (testing "a shift at least as wide as the value yields all zeros"
    (is (= "0000" (v "SELECT B'1100' << 9")))
    (is (= "0000" (v "SELECT B'1100' >> 9"))))
  (testing "a negative distance reverses direction"
    (is (= "0110" (v "SELECT B'1100' << -1")))
    (is (= "1000" (v "SELECT B'1100' >> -1")))))

(deftest persisted-bit-string-shifts
  (.execute *handler* "CREATE TABLE bit_shift (id int PRIMARY KEY, b bit(4), v varbit(4))")
  (.execute *handler* "INSERT INTO bit_shift VALUES (1, B'1100', B'0110')")
  (is (= [["1000" "0011"]]
         (mapv vec (.-rows ^PgWireServer$QueryResult
                    (.execute *handler* "SELECT b << 1, v >> 1 FROM bit_shift"))))))

(deftest postgres-bit-shift-storage-slice
  ;; PostgreSQL 17 bit.sql lines 174-179. Each INSERT reads every row written
  ;; so far, producing all right shifts from 0 through 15. PgBit used to be
  ;; stringified as a Clojure record on the second INSERT ... SELECT.
  (.execute *handler* "CREATE TABLE bit_shift_16 (b bit(16))")
  (.execute *handler* "INSERT INTO bit_shift_16 VALUES (B'1101100000000000')")
  (doseq [distance [1 2 4 8]]
    (is (nil? (err (str "INSERT INTO bit_shift_16 SELECT b >> " distance
                        " FROM bit_shift_16")))))
  (is (= 16 (Long/parseLong (v "SELECT count(*) FROM bit_shift_16"))))
  (is (= (mapv #(str (apply str (repeat % \0))
                     (subs "1101100000000000" 0 (- 16 %)))
               (range 16))
         (mapv first
               (mapv vec (.-rows ^PgWireServer$QueryResult
                          (.execute *handler*
                                    "SELECT b FROM bit_shift_16 ORDER BY b DESC")))))))

(deftest bit-column-assignment-width
  (.execute *handler* "CREATE TABLE bit_widths (b bit(4), v varbit(4))")
  (is (re-find #"bit string length 2 does not match type bit\(4\)"
               (or (err "INSERT INTO bit_widths (b) VALUES (B'11')") "")))
  (is (nil? (err "INSERT INTO bit_widths VALUES (B'11'::bit(4), B'11')")))
  (is (= [["1100" "11"]]
         (mapv vec (.-rows ^PgWireServer$QueryResult
                    (.execute *handler* "SELECT b, v FROM bit_widths")))))
  (is (re-find #"too long for type bit varying\(4\)"
               (or (err "INSERT INTO bit_widths (v) VALUES (B'11001')") ""))))

(deftest bit-access-and-count-functions
  (is (= "1" (v "SELECT get_bit(B'0101011000100', 10)")))
  (is (= "0101011000100101"
         (v "SELECT set_bit(B'0101011000100100', 15, 1)")))
  (is (re-find #"bit index 16 out of valid range \(0\.\.15\)"
               (or (err "SELECT set_bit(B'0101011000100100', 16, 1)") "")))
  (is (re-find #"new bit must be 0 or 1"
               (or (err "SELECT set_bit(B'0101', 1, 2)") "")))
  (is (= "5" (v "SELECT bit_count(B'0101011100'::bit(10))")))
  (is (= "500" (v "SELECT bit_count(repeat('01', 500)::bit(1000))")))
  (is (= [oid-int4 oid-bit oid-int8]
         (oids "SELECT get_bit(B'0', 0), set_bit(B'0', 0, 1), bit_count(B'1')"))))

(deftest bit-string-operands-must-be-the-same-width
  (testing "PG refuses rather than padding — the width is part of the value"
    (is (re-find #"cannot AND bit strings of different sizes"
                 (or (err "SELECT B'110' & B'1010'") "")))
    (is (re-find #"cannot OR bit strings of different sizes"
                 (or (err "SELECT B'110' | B'1010'") "")))))

(deftest bit-string-precedence-matches-integers
  (is (= "0010" (v "SELECT ~B'1100' & B'1010'")) "(~1100) & 1010"))

;; ---------------------------------------------------------------------------
;; Columns, not just literals
;; ---------------------------------------------------------------------------

(deftest bitwise-over-columns
  (.execute *handler* "CREATE TABLE f (id int PRIMARY KEY, flags int)")
  (.execute *handler* "INSERT INTO f VALUES (1, 12), (2, 5)")
  (is (= [["1" "4"] ["2" "4"]]
         (mapv vec (.-rows ^PgWireServer$QueryResult
                    (.execute *handler* "SELECT id, flags & 6 FROM f ORDER BY id")))))
  (testing "as a WHERE predicate — the bitmask-test idiom"
    (is (= [["1"]]
           (mapv vec (.-rows ^PgWireServer$QueryResult
                      (.execute *handler*
                                "SELECT id FROM f WHERE flags & 8 = 8")))))))

;; ---------------------------------------------------------------------------
;; `#` stays a syntax error
;; ---------------------------------------------------------------------------

(deftest hash-xor-remains-unimplemented
  (testing "JSqlParser cannot lex `#` as an operator, and emulating it
            textually could not reproduce its precedence — so it stays a
            clean 42601 rather than a wrong answer"
    (is (re-find #"syntax error at or near"
                 (or (err "SELECT 5 # 3") "")))))
