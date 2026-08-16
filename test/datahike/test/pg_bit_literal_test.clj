(ns datahike.test.pg-bit-literal-test
  "Issue #28 — `SELECT B'1001000'` must come back as bit (OID 1560), not
   text (25).

   Issue #19 gave bit *values* a real type (datahike.pg.bits), but the
   SQL literal never produced one: JSqlParser spells `B'1001000'` as a
   StringValue carrying prefix \"B\", so it fell through to the plain
   string branch and reached the client as text. `X'4A'` didn't even
   parse (`HexValue is not supported`).

   Making the literal a PgBit is most of the fix, but it also moves the
   value out of String-land, so everything that used to work on bit
   literals by string coincidence needs to hold for real: ordering,
   `||`, `bit_length`, casts, and comparison against a bit column
   (which stores PG's text form, since Datahike has no bit type).

   Expectations captured from PostgreSQL 17 by differential testing."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$PgProtocolException
            PgWireServer$QueryResult]))

(def oid-bool 16)
(def oid-int4 23)
(def oid-text 25)
(def oid-bit 1560)
(def oid-varbit 1562)

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

(defn- run [sql]
  (.execute *handler* sql))

(defn- oids [sql]
  (let [^PgWireServer$QueryResult r (run sql)]
    (vec (.-columnOids r))))

(defn- rows [sql]
  (let [^PgWireServer$QueryResult r (run sql)]
    (mapv vec (.-rows r))))

(defn- describe-oids [sql]
  (let [parsed (.parse *handler* sql (int-array 0))
        ^PgWireServer$QueryResult r (.describeResult *handler* parsed)]
    (when (and r (.-columnOids r)) (vec (.-columnOids r)))))

(defn- parse-err
  "Parse `sql` and return [sqlstate message], or nil if it parsed.
   Validation of a literal happens at parse/translate time, which is
   also where PG raises it — the wire layer turns this into the
   ErrorResponse the client sees."
  [sql]
  (try (.parse *handler* sql (int-array 0)) nil
       (catch PgWireServer$PgProtocolException e
         [(.-sqlstate e) (.getMessage e)])))

;; ---------------------------------------------------------------------------
;; The reported bug
;; ---------------------------------------------------------------------------

(deftest bit-literal-is-typed-bit-not-text
  (testing "the exact query from issue #28"
    (is (= [oid-bit] (oids "SELECT b'1001000'")))
    (is (= [["1001000"]] (rows "SELECT b'1001000'"))))
  (testing "uppercase B, and an empty bit string"
    (is (= [oid-bit] (oids "SELECT B'1001000'")))
    (is (= [oid-bit] (oids "SELECT B''")))
    (is (= [[""]] (rows "SELECT B''")))))

(deftest bit-literal-typed-at-describe-time-too
  (testing "Extended Query advertises bit before Execute runs"
    (is (= [oid-bit] (describe-oids "SELECT B'1001000'")))))

(deftest pg-typeof-reports-bit
  (is (= [["bit"]] (rows "SELECT pg_typeof(B'101')"))))

;; ---------------------------------------------------------------------------
;; Hex bit-string literals
;; ---------------------------------------------------------------------------

(deftest hex-bit-literal
  (testing "X'4A' — four bits per digit, leading zeros kept (was 0A000)"
    (is (= [oid-bit] (oids "SELECT X'4A'")))
    (is (= [["01001010"]] (rows "SELECT X'4A'"))))
  (testing "lowercase spelling and digits"
    (is (= [["01001010"]] (rows "SELECT x'4a'"))))
  (testing "a leading zero nibble is four bits, not dropped"
    (is (= [["00001111"]] (rows "SELECT X'0F'")))
    (is (= "8" (ffirst (rows "SELECT length(X'0F')")))))
  (testing "empty"
    (is (= [[""]] (rows "SELECT X''")))))

;; ---------------------------------------------------------------------------
;; Validation
;; ---------------------------------------------------------------------------

(deftest invalid-binary-digit-raises
  (testing "B'102' is 22P02, not the string \"102\""
    (let [[state msg] (parse-err "SELECT B'102'")]
      (is (= "22P02" state))
      (is (re-find #"not a valid binary digit" msg)))))

;; ---------------------------------------------------------------------------
;; Operations on bit values — these all worked by string coincidence
;; before the literal had a type, so they are the regression surface.
;; ---------------------------------------------------------------------------

(deftest bit-equality-compares-width-too
  (testing "B'101' is NOT B'10100000' — the width is part of the value"
    (is (= [["t"]] (rows "SELECT B'101' = B'101'")))
    (is (= [["f"]] (rows "SELECT B'101' = B'10100000'")))))

(deftest bit-ordering
  (testing "< / > on bit strings (PgBit must be Comparable)"
    (is (= [["t"]] (rows "SELECT B'101' < B'110'")))
    (is (= [["f"]] (rows "SELECT B'110' < B'101'")))
    (is (= [oid-bool] (oids "SELECT B'101' < B'110'"))))
  (testing "a proper prefix sorts first — B'0' < B'00'"
    (is (= [["t"]] (rows "SELECT B'0' < B'00'")))))

(deftest bit-concat
  (testing "|| is bitcat and yields bit varying, not text"
    (is (= [["10111"]] (rows "SELECT B'101' || B'11'")))
    (is (= [oid-varbit] (oids "SELECT B'101' || B'11'")))))

(deftest bit-length-functions
  (testing "length/octet_length/bit_length measure bits, not map entries"
    (is (= [["7"]] (rows "SELECT length(B'1001000')")))
    (is (= [["1"]] (rows "SELECT octet_length(B'1001000')")))
    (is (= [["7"]] (rows "SELECT bit_length(B'1001000')"))
        "bit_length was an unknown function"))
  (testing "bit_length on text is 8x the byte length"
    (is (= [["24"]] (rows "SELECT bit_length('abc')")))))

(deftest bit-casts
  (testing "::int reinterprets the bits — B'101' is 5, not 101"
    (is (= [["5"]] (rows "SELECT B'101'::int")))
    (is (= [oid-int4] (oids "SELECT B'101'::int"))))
  (testing "::varbit / ::text"
    (is (= [oid-varbit] (oids "SELECT B'101'::varbit")))
    (is (= [["101"]] (rows "SELECT B'101'::varbit")))
    (is (= [oid-text] (oids "SELECT B'101'::text")))
    (is (= [["101"]] (rows "SELECT B'101'::text")))))

;; ---------------------------------------------------------------------------
;; Bit columns
;; ---------------------------------------------------------------------------

(deftest bit-columns-report-bit-oids
  (run "CREATE TABLE bt (id int PRIMARY KEY, b bit(7), v varbit)")
  (run "INSERT INTO bt VALUES (1, B'1001000', B'101')")
  (testing "a bit column advertises bit / varbit, not text"
    (is (= [oid-int4 oid-bit oid-varbit] (oids "SELECT id, b, v FROM bt"))))
  (testing "values round-trip in PG's text form"
    (is (= [["1" "1001000" "101"]] (rows "SELECT id, b, v FROM bt"))))
  (testing "a bit literal matches a bit column — the column stores the
            digit run, so the literal has to come down to it"
    (is (= [["1"]] (rows "SELECT id FROM bt WHERE b = B'1001000'")))
    (is (= [] (rows "SELECT id FROM bt WHERE b = B'1001001'")))
    (is (= [["1"]] (rows "SELECT id FROM bt WHERE v = B'101'")))))
