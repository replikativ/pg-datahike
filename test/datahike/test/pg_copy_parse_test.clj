(ns datahike.test.pg-copy-parse-test
  "Unit coverage for `datahike.pg.sql.copy/parse-copy-from-stdin` —
   the token-driven parser for `COPY ... FROM STDIN`. Validates both
   the modern paren `WITH (key = value, ...)` and the legacy
   keyword `WITH KW1 KW2 ...` forms, plus default-resolution from
   the format keyword and rejection of the BINARY format / OIDS."
  (:require [clojure.test :refer [deftest testing is]]
            [datahike.pg.sql.copy :as copy]))

(defn- parse [sql]
  (copy/parse-copy-from-stdin (copy/tokenize sql)))

(defn- opts [sql] (:options (parse sql)))

;; ============================================================================
;; Table / column-list parsing
;; ============================================================================

(deftest parse-bare-table-name
  (let [r (parse "COPY t FROM stdin")]
    (is (nil? (:ns r)))
    (is (= "t" (:table r)))
    (is (nil? (:columns r)))))

(deftest parse-schema-qualified-name
  (let [r (parse "COPY public.users FROM stdin")]
    (is (= "public" (:ns r)))
    (is (= "users" (:table r)))))

(deftest parse-column-list
  (let [r (parse "COPY t (id, name, age) FROM stdin")]
    (is (= ["id" "name" "age"] (:columns r))))

  (testing "empty column list — () — returns empty vec, not nil"
    (let [r (parse "COPY t () FROM stdin")]
      (is (= [] (:columns r))))))

(deftest parse-column-list-lowercases
  (let [r (parse "COPY t (ID, NaMe) FROM stdin")]
    (is (= ["id" "name"] (:columns r)))))

;; ============================================================================
;; Format / defaults
;; ============================================================================

(deftest text-format-defaults
  (let [o (opts "COPY t FROM stdin")]
    (is (= :text  (:format o)))
    (is (= "\t"   (:delimiter o)))
    (is (= "\\N"  (:null-marker o)))
    (is (= :false (:header o)))))

(deftest csv-format-defaults
  (let [o (opts "COPY t FROM stdin WITH (FORMAT 'csv')")]
    (is (= :csv  (:format o)))
    (is (= ","   (:delimiter o)))
    (is (= ""    (:null-marker o)))
    (is (= "\""  (:quote o)))
    ;; ESCAPE defaults to QUOTE
    (is (= "\""  (:escape o)))))

(deftest binary-format-rejected
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"BINARY"
                        (parse "COPY t FROM stdin WITH (FORMAT 'binary')")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"BINARY"
                        (parse "COPY t FROM stdin WITH BINARY"))))

(deftest oids-rejected
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"OIDS"
                        (parse "COPY t FROM stdin WITH OIDS"))))

;; ============================================================================
;; Modern paren form
;; ============================================================================

(deftest paren-form-format-and-header
  (let [o (opts "COPY t FROM stdin WITH (FORMAT 'csv', HEADER true)")]
    (is (= :csv (:format o)))
    (is (= :true (:header o)))))

(deftest paren-form-header-match
  (let [o (opts "COPY t FROM stdin WITH (FORMAT 'csv', HEADER MATCH)")]
    (is (= :match (:header o)))))

(deftest paren-form-delimiter-null
  (let [o (opts "COPY t FROM stdin WITH (DELIMITER '|', NULL 'nil')")]
    (is (= "|" (:delimiter o)))
    (is (= "nil" (:null-marker o)))))

(deftest paren-form-quote-escape
  (let [o (opts "COPY t FROM stdin WITH (FORMAT 'csv', QUOTE '\"', ESCAPE '\\')")]
    (is (= "\"" (:quote o)))
    (is (= "\\" (:escape o)))))

(deftest paren-form-force-not-null-list
  (let [o (opts "COPY t FROM stdin WITH (FORCE_NOT_NULL (a, b))")]
    (is (= #{"a" "b"} (:force-not-null o)))))

(deftest paren-form-force-null-star
  (let [o (opts "COPY t FROM stdin WITH (FORCE_NULL *)")]
    (is (= :all (:force-null o)))))

(deftest paren-form-force-not-null-star
  (let [o (opts "COPY t FROM stdin WITH (FORCE_NOT_NULL *)")]
    (is (= :all (:force-not-null o)))))

(deftest paren-form-encoding-accepted-stored
  (let [o (opts "COPY t FROM stdin WITH (ENCODING 'UTF8')")]
    (is (= "UTF8" (:encoding o)))))

(deftest paren-form-freeze-accepted
  (is (true? (:freeze? (opts "COPY t FROM stdin WITH (FREEZE)"))))
  (is (true? (:freeze? (opts "COPY t FROM stdin WITH (FREEZE true)")))))

(deftest paren-form-default-marker
  (let [o (opts "COPY t FROM stdin WITH (DEFAULT '\\D')")]
    (is (= "\\D" (:default-marker o)))))

;; ============================================================================
;; Legacy keyword form
;; ============================================================================

(deftest legacy-bare-csv
  (let [o (opts "COPY t FROM stdin WITH CSV")]
    (is (= :csv (:format o)))
    (is (= ","  (:delimiter o)))
    (is (= ""   (:null-marker o)))))

(deftest legacy-csv-header
  (let [o (opts "COPY t FROM stdin WITH CSV HEADER")]
    (is (= :csv  (:format o)))
    (is (= :true (:header o)))))

(deftest legacy-delimiter-null-csv
  (let [o (opts "COPY t FROM stdin DELIMITER '|' NULL '' CSV")]
    (is (= :csv (:format o)))
    (is (= "|"  (:delimiter o)))
    (is (= ""   (:null-marker o)))))

(deftest legacy-no-with-keyword
  (testing "WITH is optional — pre-9.0 forms omit it"
    (let [o (opts "COPY t FROM stdin DELIMITER '|'")]
      (is (= "|" (:delimiter o))))))

(deftest legacy-force-not-null
  (let [o (opts "COPY t FROM stdin WITH CSV FORCE_NOT_NULL (a, b)")]
    (is (= :csv (:format o)))
    (is (= #{"a" "b"} (:force-not-null o)))))

;; ============================================================================
;; Validation / error paths
;; ============================================================================

(deftest unknown-option-rejected
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown COPY option: nonsense"
                        (parse "COPY t FROM stdin WITH (NONSENSE 'x')"))))

(deftest missing-from-rejected
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"expected from"
                        (parse "COPY t TO stdin"))))

(deftest copy-from-file-rejected
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"only COPY FROM STDIN"
                        (parse "COPY t FROM '/tmp/data.csv'"))))

(deftest copy-from-program-rejected
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"only COPY FROM STDIN"
                        (parse "COPY t FROM PROGRAM 'cat /tmp/data.csv'"))))

(deftest copy-query-rejected
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"COPY \( query"
                        (parse "COPY (SELECT * FROM t) TO stdout"))))

;; ============================================================================
;; Mixed real-world shapes
;; ============================================================================

(deftest real-pgdump-emitted-shape
  (testing "default pg_dump output for a table"
    (let [r (parse "COPY public.users (id, name, email) FROM stdin")]
      (is (= "public" (:ns r)))
      (is (= "users"  (:table r)))
      (is (= ["id" "name" "email"] (:columns r)))
      (is (= :text (:format (:options r)))))))

(deftest real-psql-copy-shape
  (testing "psql `\\copy t FROM 'file' DELIMITER ',' CSV HEADER` rewritten as COPY FROM stdin"
    (let [r (parse "COPY t FROM stdin WITH (FORMAT csv, DELIMITER ',', HEADER true)")]
      (is (= :csv  (:format (:options r))))
      (is (= ","   (:delimiter (:options r))))
      (is (= :true (:header (:options r)))))))
