(ns datahike.test.pg-copy-out-format-test
  "Unit coverage for `datahike.pg.sql.copy.out` -- the field encoders
   for `COPY ... TO`.

   The cases here are the ones where the writer is NOT the mirror image
   of the reader: the six control characters PostgreSQL spells in C
   notation versus the ones it passes through, and the three reasons
   CSV quotes a field that contains none of the characters quoting is
   usually about."
  (:require [clojure.test :refer [deftest testing is]]
            [datahike.pg.sql.copy.out :as out]))

(def ^:private text-opts
  {:format :text :delimiter "\t" :null-marker "\\N"})

(def ^:private csv-opts
  {:format :csv :delimiter "," :null-marker "" :quote "\"" :escape "\""})

(deftest text-escapes-only-the-six-named-control-characters
  (is (= "a\\tb" (out/text-field "a\tb" "\t" "\\N")))
  (is (= "a\\nb" (out/text-field "a\nb" "\t" "\\N")))
  (is (= "a\\rb" (out/text-field "a\rb" "\t" "\\N")))
  (is (= "a\\bb" (out/text-field "a\bb" "\t" "\\N")))
  (is (= "a\\fb" (out/text-field "a\fb" "\t" "\\N")))
  (is (= "a\\vb" (out/text-field "a\u000Bb" "\t" "\\N")))
  ;; Everything else below 0x20 is written literally. PostgreSQL
  ;; escapes those six because line-ending translation would mangle
  ;; them, not because they are control characters.
  (is (= "a\u0001b" (out/text-field "a\u0001b" "\t" "\\N")))
  (is (= "a\u001Fb" (out/text-field "a\u001Fb" "\t" "\\N"))))

(deftest text-escapes-backslash-and-whatever-the-delimiter-is
  (is (= "a\\\\b" (out/text-field "a\\b" "\t" "\\N")))
  (is (= "a\\|b" (out/text-field "a|b" "|" "\\N")))
  ;; With a non-tab delimiter a tab is still C-escaped: the switch on
  ;; the control character comes first.
  (is (= "a\\tb" (out/text-field "a\tb" "|" "\\N")))
  ;; And a character that is only special as the delimiter is plain
  ;; once it is not the delimiter.
  (is (= "a|b" (out/text-field "a|b" "\t" "\\N"))))

(deftest text-null-is-the-marker-itself
  (is (= "\\N" (out/text-field nil "\t" "\\N")))
  (is (= "" (out/text-field nil "\t" "")))
  ;; A value that merely looks like the marker is NOT distinguished in
  ;; text format -- PostgreSQL does not quote it, and reading it back
  ;; does produce NULL. That asymmetry is PostgreSQL's, not ours.
  (is (= "\\\\N" (out/text-field "\\N" "\t" "\\N"))))

(deftest csv-quotes-only-when-it-must
  (is (= "plain" (out/csv-field "plain" "," "\"" "\"" "" {})))
  (is (= "\"a,b\"" (out/csv-field "a,b" "," "\"" "\"" "" {})))
  (is (= "\"a\"\"b\"" (out/csv-field "a\"b" "," "\"" "\"" "" {})))
  (is (= "\"a\nb\"" (out/csv-field "a\nb" "," "\"" "\"" "" {})))
  (is (= "\"a\rb\"" (out/csv-field "a\rb" "," "\"" "\"" "" {}))))

(deftest csv-quotes-what-would-otherwise-read-back-wrong
  (testing "a value equal to the NULL marker"
    ;; Unquoted it would come back as NULL, so it is quoted -- while
    ;; an actual NULL is written raw. That is the whole distinction
    ;; between an empty field and an empty string in CSV.
    (is (= "\"\"" (out/csv-field "" "," "\"" "\"" "" {})))
    (is (= "" (out/csv-field nil "," "\"" "\"" "" {})))
    (is (= "\"NULL\"" (out/csv-field "NULL" "," "\"" "\"" "NULL" {})))
    (is (= "NULL" (out/csv-field nil "," "\"" "\"" "NULL" {}))))
  (testing "a lone \\. in a single-column file"
    ;; An older reader would take it for the end-of-data marker.
    (is (= "\"\\.\"" (out/csv-field "\\." "," "\"" "\"" "" {:single-attr? true})))
    (is (= "\\." (out/csv-field "\\." "," "\"" "\"" "" {:single-attr? false}))))
  (testing "FORCE_QUOTE"
    (is (= "\"x\"" (out/csv-field "x" "," "\"" "\"" "" {:force-quote? true})))))

(deftest csv-escape-char-can-differ-from-the-quote-char
  ;; With ESCAPE '\' a quote is backslash-escaped rather than doubled,
  ;; and the escape char itself is escaped too.
  (is (= "\"a\\\"b\"" (out/csv-field "a\"b" "," "\"" "\\" "" {})))
  (is (= "\"a,\\\\b\"" (out/csv-field "a,\\b" "," "\"" "\\" "" {}))))

(deftest row-line-joins-with-the-delimiter
  (is (= "1\tada\t\\N"
         (out/row-line ["1" "ada" nil] ["id" "name" "email"] text-opts)))
  (is (= "1,\"a,b\","
         (out/row-line ["1" "a,b" nil] ["id" "name" "email"] csv-opts))))

(deftest row-line-applies-force-quote-by-column-name
  (is (= "1,\"ada\""
         (out/row-line ["1" "ada"] ["id" "name"]
                       (assoc csv-opts :force-quote #{"name"}))))
  (is (= "\"1\",\"ada\""
         (out/row-line ["1" "ada"] ["id" "name"]
                       (assoc csv-opts :force-quote :all))))
  ;; FORCE_QUOTE never applies to a NULL: it is the marker, written raw.
  (is (= "\"1\","
         (out/row-line ["1" nil] ["id" "name"]
                       (assoc csv-opts :force-quote :all)))))
