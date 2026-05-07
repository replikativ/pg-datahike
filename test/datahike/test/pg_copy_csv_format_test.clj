(ns datahike.test.pg-copy-csv-format-test
  "Unit coverage for `datahike.pg.sql.copy.csv-format` — the
   PostgreSQL COPY-IN CSV-format decoder.

   Spec source:
   `../postgres/src/backend/commands/copyfromparse.c:1827`
   (CopyReadAttributesCSV) and the WITH-options grammar in
   `../postgres/doc/src/sgml/ref/copy.sgml`."
  (:require [clojure.test :refer [deftest testing is]]
            [datahike.pg.sql.copy.csv-format :as csv]))

(def ^:private csv-defaults
  "PG's CSV-mode defaults from copy.c:763-773."
  {:delimiter "," :null-marker "" :quote "\"" :escape "\""
   :header :false})

(defn- decode
  ([chunks] (decode csv-defaults chunks))
  ([opts chunks] (csv/decode-all opts (if (sequential? chunks) chunks [chunks]))))

;; ============================================================================
;; Basic rows
;; ============================================================================

(deftest single-row
  (is (= [["a" "b" "c"]] (decode "a,b,c\n"))))

(deftest multiple-rows
  (is (= [["a" "b"] ["c" "d"]] (decode "a,b\nc,d\n"))))

(deftest no-trailing-newline-emits-final-row
  (is (= [["a" "b"]] (decode "a,b"))))

;; ============================================================================
;; Quoting
;; ============================================================================

(deftest quoted-empty-field-is-empty-string-not-null
  (testing "Empty quoted field → empty string (CSV-default null = empty)"
    ;; default null-marker is "" — but the field WAS quoted, so it's
    ;; empty string (not null)
    (is (= [["" "b"]] (decode "\"\",b\n")))
    ;; Bare empty (no quotes) with null-marker = "" → null
    (is (= [[:datahike.pg.sql.copy.csv-format/null "b"]]
           (decode ",b\n")))))

(deftest quoted-field-with-comma
  (testing "Comma inside quotes is data, not a delimiter"
    (is (= [["hello, world" "next"]]
           (decode "\"hello, world\",next\n")))))

(deftest quoted-field-with-newline
  (testing "Newline inside quotes is data, not a row terminator"
    (is (= [["line1\nline2" "next"]]
           (decode "\"line1\nline2\",next\n")))))

(deftest doubled-quote-inside-quotes
  (testing "\"\" inside quoted field → literal \" (default escape = quote)"
    (is (= [["she said \"hi\""]]
           (decode "\"she said \"\"hi\"\"\"\n")))))

(deftest quote-and-escape-different
  (testing "ESCAPE different from QUOTE — backslash escape style"
    (is (= [["she said \"hi\""]]
           (decode (assoc csv-defaults :escape "\\")
                   ["\"she said \\\"hi\\\"\"\n"])))))

;; ============================================================================
;; NULL detection (only on unquoted fields by default)
;; ============================================================================

(deftest null-marker-default-empty-string
  (testing "Default CSV null-marker is empty string; bare unquoted empties → null"
    (is (= [[:datahike.pg.sql.copy.csv-format/null
             :datahike.pg.sql.copy.csv-format/null
             :datahike.pg.sql.copy.csv-format/null]]
           (decode ",,\n")))))

(deftest custom-null-marker
  (testing "NULL 'NIL' — only unquoted NIL becomes null"
    (let [opts (assoc csv-defaults :null-marker "NIL")]
      (is (= [[:datahike.pg.sql.copy.csv-format/null "data"]]
             (csv/decode-all opts ["NIL,data\n"])))
      ;; Quoted NIL → literal string "NIL", not null
      (is (= [["NIL" "data"]]
             (csv/decode-all opts ["\"NIL\",data\n"]))))))

(deftest force-not-null-keeps-quoted-empty-as-empty-string
  (testing "FORCE_NOT_NULL on a column — bare empty is treated as empty string, not null"
    (let [opts (assoc csv-defaults
                      :columns ["a" "b"]
                      :force-not-null #{"a"})]
      (is (= [["" "b"]]
             (csv/decode-all opts [",b\n"]))))))

(deftest force-not-null-star-applies-everywhere
  (let [opts (assoc csv-defaults :force-not-null :all)]
    (is (= [["" "" ""]]
           (csv/decode-all opts [",,\n"])))))

(deftest force-null-makes-quoted-match-null
  (testing "FORCE_NULL — a quoted value matching null-marker DOES become null"
    (let [opts (assoc csv-defaults
                      :null-marker "NIL"
                      :columns ["a" "b"]
                      :force-null #{"a"})]
      ;; Quoted "NIL" in column a → null (FORCE_NULL kicks in)
      (is (= [[:datahike.pg.sql.copy.csv-format/null "data"]]
             (csv/decode-all opts ["\"NIL\",data\n"])))
      ;; Quoted "NIL" in column b (no FORCE_NULL) → literal string
      (is (= [["data" "NIL"]]
             (csv/decode-all opts ["data,\"NIL\"\n"]))))))

;; ============================================================================
;; Custom delimiter
;; ============================================================================

(deftest pipe-delimiter
  (is (= [["a" "b" "c"]]
         (decode (assoc csv-defaults :delimiter "|") ["a|b|c\n"]))))

(deftest semicolon-delimiter
  (is (= [["a" "b" "c"]]
         (decode (assoc csv-defaults :delimiter ";") ["a;b;c\n"]))))

;; ============================================================================
;; HEADER handling
;; ============================================================================

(deftest header-true-skips-first-row
  (let [opts (assoc csv-defaults :header :true)]
    (is (= [["1" "alice"] ["2" "bob"]]
           (csv/decode-all opts ["id,name\n1,alice\n2,bob\n"])))))

(deftest header-false-doesnt-skip
  (let [opts (assoc csv-defaults :header :false)]
    (is (= [["id" "name"] ["1" "alice"]]
           (csv/decode-all opts ["id,name\n1,alice\n"])))))

(deftest header-match-validates
  (testing "HEADER MATCH: header row must match :columns"
    (let [opts (assoc csv-defaults
                      :header :match
                      :columns ["id" "name"])]
      (is (= [["1" "alice"]]
             (csv/decode-all opts ["id,name\n1,alice\n"])))
      ;; Mismatch — error
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"HEADER MATCH"
                            (csv/decode-all opts ["wrongcol,othercol\n1,alice\n"]))))))

;; ============================================================================
;; Streaming / chunk boundaries
;; ============================================================================

(deftest chunk-boundary-mid-quoted-field
  (testing "Quoted field split across chunks"
    (is (= [["hello, world" "next"]]
           (decode ["\"hello, " "world\"," "next\n"])))))

(deftest chunk-boundary-mid-doubled-quote
  (testing "\"\" escape spanning chunks"
    (is (= [["she said \"hi\""]]
           (decode ["\"she said \"" "\"hi\"" "\"\"\n"])))))

(deftest chunk-boundary-at-cr
  (testing "CR at end of one chunk, LF starts next — recognized as CRLF"
    (is (= [["a"] ["b"]]
           (decode ["a\r" "\nb\r\n"])))))

;; ============================================================================
;; Backslash is a literal character in CSV
;; ============================================================================

(deftest backslash-is-literal-in-csv
  (testing "Backslash + char passes through verbatim (no escape interpretation)"
    (is (= [["foo\\nbar"]] (decode "foo\\nbar\n")))
    (is (= [["a\\tb"]]     (decode "a\\tb\n")))))

(deftest backslash-dot-is-not-eod-in-csv
  (testing "\\. inside CSV stream is just a literal field, not EOD"
    (is (= [["\\." "data"]] (decode "\\.,data\n")))))

;; ============================================================================
;; Real psql-emitted shape
;; ============================================================================

(deftest pgdump-csv-emit-with-mixed-fields
  (let [opts csv-defaults
        body (str "1,alice,\"line1\nline2\",,\"hello, world\"\n"
                  "2,bob,plain,nullval,\"emb\"\"quote\"\n")
        rows (decode opts body)]
    (is (= 2 (count rows)))
    (is (= ["1" "alice" "line1\nline2"
            :datahike.pg.sql.copy.csv-format/null
            "hello, world"]
           (first rows)))
    (is (= ["2" "bob" "plain" "nullval" "emb\"quote"]
           (second rows)))))

(deftest large-csv-fixture
  (let [n 500
        opts csv-defaults
        body (apply str
                    (for [i (range n)]
                      (format "%d,name-%d,\"hello, world %d\"\n" i i i)))
        rows (decode opts body)]
    (is (= n (count rows)))
    (is (= ["0" "name-0" "hello, world 0"] (first rows)))
    (is (= [(str (dec n)) (str "name-" (dec n)) (str "hello, world " (dec n))]
           (last rows)))))
