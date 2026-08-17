(ns datahike.test.pg-string-literal-test
  "String-literal escape semantics.

   With `standard_conforming_strings = on` — which we report — a
   backslash in an ORDINARY single-quoted literal is a literal
   backslash. Only `E'...'` strings process escapes.

   We had both halves wrong, and in the same place: the wire layer's
   `stripComments` rewrote the SQL text before it was executed.

     - `\\n` / `\\r` / `\\t` inside a literal became a SPACE — not even
       the control character it was pretending to decode. So
       `length('a\\tb')` was 3 (PG: 4) and `'C:\\temp'` came back as
       `C: emp`, 6 characters instead of 7.
     - A REAL tab or newline inside a literal became a space too, on
       the grounds that jsqlparser could not lex them. It can.

   And with those removed, `E'...'` needed real decoding, which had
   never existed — it had only ever looked right because the space
   substitution happened to collapse two characters into one.

   Expectations captured from PostgreSQL 17."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.expr :as expr])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *h* nil)

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (binding [*h* (pg/make-query-handler conn)] (f))
           (finally (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- run [sql] (.execute *h* sql))
(defn- v [sql] (ffirst (mapv vec (.-rows ^PgWireServer$QueryResult (run sql)))))
(defn- err [sql] (.-error ^PgWireServer$QueryResult (run sql)))

;; ---------------------------------------------------------------------------
;; Ordinary literals: a backslash is just a backslash

(deftest ordinary-literals-do-not-process-escapes
  (testing "backslash-t is two characters"
    (is (= "4" (v "SELECT length('a\\tb')")))
    (is (= "a\\tb" (v "SELECT 'a\\tb'"))))

  (testing "a Windows path keeps its separator"
    (is (= "7" (v "SELECT length('C:\\temp')")))
    (is (= "C:\\temp" (v "SELECT 'C:\\temp'"))))

  (testing "backslash-n and backslash-r likewise"
    (is (= "4" (v "SELECT length('a\\nb')")))
    (is (= "4" (v "SELECT length('a\\rb')"))))

  (testing "a doubled quote is still one quote"
    (is (= "it's" (v "SELECT 'it''s'")))
    (is (= "4" (v "SELECT length('it''s')")))))

(deftest a-real-tab-in-a-literal-survives
  ;; Was replaced with a space because "JSqlParser's lexer cannot handle
  ;; these inside string literals". jsqlparser 5.2 handles both.
  (is (= "3" (v (str "SELECT length('a" (char 9) "b')"))))
  (is (= "3" (v (str "SELECT length('a" (char 10) "b')")))))

;; ---------------------------------------------------------------------------
;; E'...' — the only form that DOES process escapes

(deftest e-strings-process-escapes
  (testing "the single-character set: b f n r t v"
    (is (= "3" (v "SELECT length(E'a\\tb')")))
    (is (= "1" (v "SELECT length(E'\\v')")))
    (is (= "3" (v "SELECT length(E'a\\nb')"))))

  (testing "a doubled backslash is one backslash"
    (is (= "3" (v "SELECT length(E'a\\\\b')")))
    (is (= "a\\b" (v "SELECT E'a\\\\b'"))))

  (testing "hex, octal and unicode"
    (is (= "A" (v "SELECT E'\\x41'")))
    (is (= "A" (v "SELECT E'\\101'")))
    (is (= "A" (v "SELECT E'\\u0041'")))
    (is (= "aAb" (v "SELECT E'a\\x41b'"))))

  (testing "an unknown escape drops the backslash — PG's documented fallthrough"
    (is (= "aqb" (v "SELECT E'a\\qb'")))
    (is (= "3"   (v "SELECT length(E'a\\qb')"))))

  (testing "a doubled quote works inside E'' too"
    (is (= "it's" (v "SELECT E'it''s'"))))

  (testing "E'' and an ordinary literal agree once escapes are applied"
    (is (= "t" (v "SELECT 'a' || E'\\t' || 'b' = E'a\\tb'")))))

(deftest e-string-errors-match-postgres
  (testing "a NUL code point is rejected, whatever the escape form"
    (doseq [lit ["E'\\0'" "E'\\x00'" "E'\\u0000'"]]
      (is (re-find #"invalid byte sequence" (or (err (str "SELECT " lit)) ""))
          lit)))

  (testing "a short unicode escape is an error, not a fallthrough"
    (is (re-find #"invalid Unicode escape" (or (err "SELECT E'\\u00'") "")))
    (is (re-find #"invalid Unicode escape" (or (err "SELECT E'\\U0041'") "")))))

;; ---------------------------------------------------------------------------
;; A constant-only SELECT takes a fast path that used to bypass the decoder

(deftest the-constant-select-fast-path-decodes-too
  ;; `SELECT E'a\qb'` alone went through a literal fast path in sql.clj
  ;; that read .getNotExcapedValue directly, so it returned the RAW body
  ;; while `SELECT E'a\qb', 1` — which takes the general path — decoded.
  (is (= "A"   (v "SELECT E'\\x41'")))
  (is (= "A"   (v "SELECT E'\\x41', 1")))
  (is (= "aqb" (v "SELECT E'a\\qb'")))
  (is (= "aqb" (v "SELECT E'a\\qb', 1"))))

;; ---------------------------------------------------------------------------
;; Round-trip through storage

(deftest escapes-round-trip-through-a-column
  (run "CREATE TABLE s (id int, t text)")
  (run "INSERT INTO s VALUES (1, 'C:\\temp'), (2, E'a\\tb')")
  (is (= "7" (v "SELECT length(t) FROM s WHERE id = 1")))
  (is (= "C:\\temp" (v "SELECT t FROM s WHERE id = 1")))
  (is (= "3" (v "SELECT length(t) FROM s WHERE id = 2"))))

;; ---------------------------------------------------------------------------
;; The decoder in isolation

(deftest decoder-cases
  (doseq [[in out] {"a\\tb"    "a\tb"
                    "a\\\\b"   "a\\b"
                    "\\x41"    "A"
                    "\\101"    "A"
                    "\\u0041"  "A"
                    "a\\qb"    "aqb"
                    "it''s"    "it's"
                    "\\x4"     "\u0004"
                    "plain"    "plain"}]
    (is (= out (expr/decode-e-string in)) (pr-str in))))
