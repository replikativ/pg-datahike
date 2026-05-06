(ns datahike.test.pg-copy-text-format-test
  "Unit coverage for `datahike.pg.sql.copy.text-format` — the
   PostgreSQL COPY-IN text-format decoder.

   Spec source: `../postgres/doc/src/sgml/ref/copy.sgml`. Test cases
   are derived from the documented escape sequences plus shape
   examples from `pg_dump` output."
  (:require [clojure.test :refer [deftest testing is]]
            [datahike.pg.sql.copy.text-format :as tf]))

(def ^:private text-opts
  {:delimiter "\t" :null-marker "\\N"})

(defn- decode [chunks]
  (tf/decode-all text-opts (if (sequential? chunks) chunks [chunks])))

;; ============================================================================
;; Basic line / field splitting
;; ============================================================================

(deftest single-row-basic
  (is (= [["a" "b" "c"]] (decode "a\tb\tc\n"))))

(deftest multiple-rows
  (is (= [["a" "b"] ["c" "d"] ["e" "f"]]
         (decode "a\tb\nc\td\ne\tf\n"))))

(deftest no-trailing-newline-is-fine
  (testing "Final line without terminator is still emitted"
    (is (= [["a" "b"]] (decode "a\tb")))))

(deftest empty-input
  (is (= [] (decode ""))))

(deftest empty-row-single-empty-string
  (testing "Empty line — one row of one empty field"
    (is (= [[""]] (decode "\n")))))

;; ============================================================================
;; NULL handling
;; ============================================================================

(deftest null-marker-default
  (is (= [["a" :datahike.pg.sql.copy.text-format/null "c"]]
         (decode "a\t\\N\tc\n"))))

(deftest null-marker-custom
  (testing "Custom null marker (e.g. NULL '')"
    (is (= [["a" :datahike.pg.sql.copy.text-format/null]]
           (tf/decode-all {:delimiter "\t" :null-marker ""} ["a\t\n"])))))

(deftest empty-string-vs-null
  (testing "With null-marker = \\N, empty string is empty string, not null"
    (is (= [["a" "" "c"]] (decode "a\t\tc\n")))))

(deftest leading-spaces-preserved
  (testing "Leading whitespace inside a field is preserved (no SQL trim)"
    (is (= [["a" "" " c"]] (decode "a\t\t c\n")))))

;; ============================================================================
;; Backslash escapes (table from copy.sgml:735)
;; ============================================================================

(deftest backslash-escape-control-chars
  (testing "\\b \\f \\n \\r \\t \\v"
    (is (= [["\b"]] (decode "\\b\n")))
    (is (= [["\f"]] (decode "\\f\n")))
    (is (= [["\n"]] (decode "\\n\n")))
    (is (= [["\r"]] (decode "\\r\n")))
    (is (= [["\t"]] (decode "\\t\n")))
    (is (= [[(str (char 11))]] (decode "\\v\n")))))

(deftest backslash-octal-1-to-3-digits
  (testing "\\NNN — 1 to 3 octal digits"
    (is (= [["\\7"]] (decode (str "\\\\" "7" "\n"))))   ;; literal \7 (escaped backslash)
    (is (= [[(str (char 7))]]   (decode "\\7\n")))      ;; bell (octal 7)
    (is (= [[(str (char 65))]]  (decode "\\101\n")))    ;; 'A' (octal 101)
    (is (= [[(str (char 8))]]   (decode "\\010\n")))))  ;; backspace (octal 010)

(deftest backslash-hex-1-to-2-digits
  (testing "\\xNN — 1 to 2 hex digits"
    (is (= [[(str (char 0x41))]] (decode "\\x41\n")))   ;; 'A'
    (is (= [[(str (char 0xc))]]  (decode "\\xc\n")))    ;; form feed
    (is (= [[(str (char 0xab))]] (decode "\\xab\n")))   ;; lowercase
    (is (= [[(str (char 0xAB))]] (decode "\\xAB\n")))   ;; uppercase
    ))

(deftest backslash-other-char-passes-through
  (testing "\\<other> — backslash + arbitrary char passes the char through"
    (is (= [["q"]] (decode "\\q\n")))
    (is (= [["@"]] (decode "\\@\n")))
    (is (= [["A"]] (decode "\\A\n")))))

(deftest backslash-backslash-is-literal
  (testing "\\\\ — escaped backslash"
    (is (= [["\\"]] (decode "\\\\\n")))))

(deftest backslash-x-no-hex-digits
  (testing "\\x with no following hex digits passes 'x' through literally"
    ;; Spec: "Any other backslashed character that is not mentioned in
    ;; the above table will be taken to represent itself."
    (is (= [["x"]] (decode "\\x\n")))
    (is (= [["xy"]] (decode "\\xy\n")))))

(deftest delimiter-must-be-escaped-as-data
  (testing "\\<delim> in data, escaped as \\\\t (tab) → field with literal tab"
    (is (= [["foo\tbar"]] (decode "foo\\tbar\n"))))

  (testing "Backslash-escaped backslash followed by tab → literal backslash + delim"
    (is (= [["foo\\" "bar"]] (decode "foo\\\\\tbar\n")))))

;; ============================================================================
;; End-of-data marker
;; ============================================================================

(deftest eod-marker-terminates-stream
  (testing "Line containing exactly \\. — bytes after are discarded"
    (is (= [["a" "b"]]
           (decode "a\tb\n\\.\nignored\trow\n")))))

(deftest eod-only-recognized-on-its-own-line
  (testing "\\. inside a row is just \\. (since \\. isn't a recognized escape)"
    ;; PG spec: "any other backslashed character is taken to represent
    ;; itself" → \. inside a field should pass `.` through.
    (is (= [["foo.bar"]] (decode "foo\\.bar\n")))))

;; ============================================================================
;; Line terminators
;; ============================================================================

(deftest lf-terminator
  (is (= [["a"] ["b"]] (decode "a\nb\n"))))

(deftest crlf-terminator
  (is (= [["a"] ["b"]] (decode "a\r\nb\r\n"))))

(deftest cr-only-terminator
  (is (= [["a"] ["b"]] (decode "a\rb\r"))))

(deftest mixed-eol-rejected
  (testing "PG enforces consistent EOL once locked in"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"inconsistent EOL"
                          (decode "a\nb\rc\n")))))

;; ============================================================================
;; Streaming / chunk boundaries
;; ============================================================================

(deftest chunk-boundary-mid-line
  (testing "Chunk split mid-row — row not emitted until terminator arrives"
    (is (= [["alice" "bob"]]
           (decode ["ali" "ce" "\t" "bob" "\n"])))))

(deftest chunk-boundary-mid-escape
  (testing "Backslash escape spanning chunks"
    (is (= [["\n"]]
           (decode ["\\" "n\n"])))))

(deftest chunk-boundary-at-crlf
  (testing "CR in one chunk, LF in next — recognized as one CRLF"
    (is (= [["a"] ["b"]]
           (decode ["a\r" "\nb\r\n"])))))

(deftest many-rows-streamed
  (let [chunks (mapv #(format "row%d\tval%d\n" % %) (range 100))
        decoded (decode chunks)]
    (is (= 100 (count decoded)))
    (is (= ["row0" "val0"]   (first decoded)))
    (is (= ["row99" "val99"] (last decoded)))))

;; ============================================================================
;; Real pg_dump-emitted shape
;; ============================================================================

(deftest pgdump-shape-with-escapes
  (testing "Sample row from `pg_dump` against a row with newlines + nulls"
    (is (= [["1" "alice" :datahike.pg.sql.copy.text-format/null "line1\nline2"]]
           (decode "1\talice\t\\N\tline1\\nline2\n")))))

(deftest pgdump-large-fixture
  (let [n 500
        body (apply str
                    (for [i (range n)]
                      (format "%d\tname-%d\temail%d@example.com\n" i i i)))
        rows (decode body)]
    (is (= n (count rows)))
    (is (= ["0" "name-0" "email0@example.com"] (first rows)))
    (is (= [(str (dec n)) (str "name-" (dec n)) (str "email" (dec n) "@example.com")]
           (last rows)))))

;; ============================================================================
;; Custom delimiters
;; ============================================================================

(deftest custom-delimiter-pipe
  (is (= [["a" "b" "c"]]
         (tf/decode-all {:delimiter "|" :null-marker "\\N"}
                        ["a|b|c\n"]))))

(deftest delimiter-must-be-escaped-when-data
  (testing "If delimiter is `|`, a literal `|` in data must be `\\|`"
    (is (= [["a|b" "c"]]
           (tf/decode-all {:delimiter "|" :null-marker "\\N"}
                          ["a\\|b|c\n"])))))
