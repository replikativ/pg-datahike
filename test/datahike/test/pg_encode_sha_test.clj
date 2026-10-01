(ns datahike.test.pg-encode-sha-test
  "`encode`/`decode` (encode.c) and the SHA-2 family, plus the
   SQL-standard `RETURN expr` function body.

   These three are one errand. PostgreSQL's `test_setup` -- which runs
   before EVERY regression file -- defines

       create function fipshash(bytea) returns text
           strict immutable parallel safe leakproof
           return substr(encode(sha256($1), 'hex'), 1, 32);

   and twelve other regression files call it. All three pieces were
   missing: `encode`, `sha256`, and the `RETURN expr` body form.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0 :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"enc" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port* "/enc?user=x&password=x&sslmode=disable")))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- state-of [^Connection c sql]
  (try (one c sql) nil (catch SQLException e (.getSQLState e))))

(defn- message-of [^Connection c sql]
  (try (one c sql) nil (catch SQLException e (.getMessage e))))

(deftest encode_has_three_formats
  (with-open [c (jdbc)]
    (is (= "616263" (one c "select encode('abc'::bytea,'hex')")))
    (is (= "YWJj" (one c "select encode('abc'::bytea,'base64')")))
    (testing "escape octal-escapes a NUL and a high-bit byte, doubles a
              backslash, and writes every other byte as itself -- the
              ASCII control characters included, which the name does not
              suggest"
      (is (= "\\000\\\\\u007f\\200\\377A"
             (one c "select encode(decode('005c7f80ff41','hex'),'escape')"))))))

(deftest base64_breaks_lines_at_76
  ;; `pg_base64_encode` emits a newline every 76 characters. Nothing
  ;; else in the encoder does, and omitting it makes every value over
  ;; 57 input bytes disagree.
  (with-open [c (jdbc)]
    (is (= 109 (count (one c "select encode(decode(repeat('61',80),'hex'),'base64')"))))
    (is (= "t" (one c (str "select decode(encode(decode(repeat('61',80),'hex'),"
                           "'base64'),'base64') = decode(repeat('61',80),'hex')"))))))

(deftest decode_is_the_inverse
  (with-open [c (jdbc)]
    (is (= "\\x616263" (one c "select decode('616263','hex')")))
    (is (= "\\x616263" (one c "select decode('YWJj','base64')")))
    (is (= "\\x61620163" (one c "select decode('ab\\001c','escape')")))))

(deftest decode_names_what_it_choked_on
  ;; Both hex errors are 22023 in `get_hex` and its caller -- NOT the
  ;; 22P02 a malformed bytea LITERAL gets. The distinction is
  ;; PostgreSQL's, and a message that names the digit is the difference
  ;; between a usable error and a shrug.
  (with-open [c (jdbc)]
    (is (= "22023" (state-of c "select decode('zz','hex')")))
    (is (re-find #"invalid hexadecimal digit: \"z\""
                 (message-of c "select decode('zz','hex')")))
    (is (= "22023" (state-of c "select decode('abc','hex')")))
    (is (re-find #"odd number of digits" (message-of c "select decode('abc','hex')")))
    (is (re-find #"invalid symbol \"!\" found while decoding base64"
                 (message-of c "select decode('a!b=','base64')")))
    (is (= "22023" (state-of c "select encode('abc'::bytea,'nope')")))))

(deftest sha2_family
  (with-open [c (jdbc)]
    (is (= "\\x23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7"
           (one c "select sha224('abc'::bytea)")))
    (is (= "\\xba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
           (one c "select sha256('abc'::bytea)")))
    (is (= (str "\\xcb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed"
                "8086072ba1e7cc2358baeca134c825a7")
           (one c "select sha384('abc'::bytea)")))
    (is (= (str "\\xddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a"
                "2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f")
           (one c "select sha512('abc'::bytea)")))))

(deftest return_expression_is_a_function_body
  ;; The SQL-standard body: no LANGUAGE, no AS. PostgreSQL 14+ reads it
  ;; as LANGUAGE SQL.
  (with-open [c (jdbc)]
    (exec! c "create function f1(int) returns int return $1 + 1")
    (is (= "3" (one c "select f1(2)")))
    (testing "its $1 is a placeholder in a TEMPLATE, like PREPARE's"
      ;; `AS $$ ... $1 ... $$` hides the parameter inside a
      ;; dollar-quoted string, so the tokeniser never saw one and that
      ;; spelling always worked. `RETURN $1` leaves it bare, and the
      ;; simple-query guard rejected the statement with 42P02 -- two
      ;; spellings of one function disagreeing.
      (exec! c "create function f2(int) returns int as $$ select $1 + 1 $$ language sql")
      (is (= (one c "select f1(7)") (one c "select f2(7)"))))))

(deftest fipshash_as_test_setup_defines_it
  (with-open [c (jdbc)]
    (exec! c (str "create function fipshash(bytea) returns text "
                  "strict immutable parallel safe leakproof "
                  "return substr(encode(sha256($1), 'hex'), 1, 32)"))
    (exec! c (str "create function fipshash(text) returns text "
                  "strict immutable parallel safe leakproof "
                  "return substr(encode(sha256($1::bytea), 'hex'), 1, 32)"))
    (is (= "ba7816bf8f01cfea414140de5dae2223" (one c "select fipshash('abc'::bytea)")))
    (is (= "ba7816bf8f01cfea414140de5dae2223" (one c "select fipshash('abc'::text)")))))

(deftest bytea-cast-is-not-a-no-op
  ;; `::bytea` had no branch in the expression-level cast, so it
  ;; returned the TEXT unchanged and every bytea function then worked on
  ;; the characters of the literal. The write path DID reach
  ;; `cast-scalar`, so a bytea through a column was right and the same
  ;; value written as a literal was not -- two paths, one tested.
  (with-open [c (jdbc)]
    (is (= "3" (one c "SELECT length('\\x616263'::bytea)")))
    (is (= "3" (one c "SELECT octet_length('\\x616263'::bytea)")))
    (is (= "616263" (one c "SELECT encode('\\x616263'::bytea, 'hex')")))
    (is (= "\\x616263" (one c "SELECT '\\x616263'::bytea")))
    (testing "the digests agree with a 17 oracle, which is the whole
              point of fipshash -- it was returning a plausible wrong
              hash of the eight literal characters"
      (is (= "900150983cd24fb0d6963f7d28e17f72"
             (one c "SELECT md5('\\x616263'::bytea)")))
      (is (= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
             (one c "SELECT encode(sha256('\\x616263'::bytea), 'hex')")))
      (testing "and md5(text) is unchanged -- PostgreSQL hashes the UTF-8
                bytes either way, so one coercion serves both"
        (is (= "900150983cd24fb0d6963f7d28e17f72" (one c "SELECT md5('abc')")))))
    (testing "md5 of a bytea COLUMN hashed the byte array's Java
              toString, so the digest changed on every run"
      (exec! c "CREATE TABLE bt (b bytea)")
      (exec! c "INSERT INTO bt VALUES ('\\x616263')")
      (is (= "900150983cd24fb0d6963f7d28e17f72" (one c "SELECT md5(b) FROM bt"))))))

(deftest byteain-escape-format
  ;; The traditional escaped style was not implemented: a string that
  ;; did not begin `\x` had its UTF-8 bytes taken verbatim, so the
  ;; escapes were stored as themselves and a lone backslash -- which
  ;; PostgreSQL rejects -- was accepted.
  (with-open [c (jdbc)]
    (is (= "\\x61" (one c "SELECT '\\141'::bytea")))
    (is (= "\\x4142" (one c "SELECT '\\101\\102'::bytea")))
    (is (= "\\x5c" (one c "SELECT '\\\\'::bytea")))
    (is (= "\\x610062" (one c "SELECT 'a\\000b'::bytea")))
    (is (= "\\x616263" (one c "SELECT 'abc'::bytea")))
    (testing "a lone backslash, followed by neither another nor three
              valid octal digits, is 22P02"
      (is (= "22P02" (state-of c "SELECT '\\1'::bytea")))
      (is (= "22P02" (state-of c "SELECT 'ab\\'::bytea")))
      (testing "\\400 is out of byteain's [0-3][0-7][0-7] range"
        (is (= "22P02" (state-of c "SELECT '\\400'::bytea")))))))

(deftest byteain-hex-format
  (with-open [c (jdbc)]
    (is (= "0" (one c "SELECT length('\\x'::bytea)")))
    (testing "whitespace is skipped BETWEEN pairs"
      (is (= "\\x6162" (one c "SELECT '\\x61 62'::bytea"))))
    (testing "but not inside one -- hex_decode_safe skips at the top of a
              loop that consumes two digits"
      (is (= "22023" (state-of c "SELECT '\\x6 162'::bytea"))))
    (testing "both hex errors are 22023, not the 22P02 the escaped style
              raises; a bad digit is quoted and an odd count is named"
      (is (= "22023" (state-of c "SELECT '\\xZZ'::bytea")))
      (is (= "22023" (state-of c "SELECT '\\x616'::bytea")))
      (is (= "ERROR: invalid hexadecimal digit: \"Z\""
             (message-of c "SELECT '\\xZZ'::bytea")))
      (is (= "ERROR: invalid hexadecimal data: odd number of digits"
             (message-of c "SELECT '\\x616'::bytea"))))))
