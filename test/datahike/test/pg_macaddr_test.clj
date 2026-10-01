(ns datahike.test.pg-macaddr-test
  "`macaddr` (EUI-48) and `macaddr8` (EUI-64), from `mac.c` and `mac8.c`.

   Neither could be spelled. Their `pg_type` rows were generated from
   pg_type.dat, but nothing mapped the NAMES, so
   `'08:00:2b:01:02:03'::macaddr` was `type \"macaddr\" does not exist`
   -- and so was a column of one, which takes the rest of a file with
   it.

   Held as canonical text, which for these costs nothing: the canonical
   form is fixed-width lower-case hex, so lexicographic text order IS
   `macaddr_cmp`'s unsigned byte order and no comparator is needed.

   The two input grammars are NOT the same, and that is the thing to get
   right. `macaddr_in` is seven fixed `sscanf` patterns whose group
   widths differ. `macaddr8_in` is a scanner: hex pairs with ONE
   separator used consistently, so groups may be any even width.

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
          {:keys [server]} (pg/start-server {"mac" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port* "/mac?user=x&password=x&sslmode=disable")))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- state-of [^Connection c sql]
  (try (one c sql) nil (catch SQLException e (.getSQLState e))))

(deftest macaddr-accepts-all-seven-spellings
  ;; `macaddr_in`'s seven `sscanf` patterns. The group WIDTHS differ
  ;; between them -- `%x:%x:…` takes any width, `%2x%2x%2x:%2x%2x%2x`
  ;; exactly two -- so this is not one split with a choice of separator.
  (with-open [c (jdbc)]
    (doseq [spelling ["08:00:2b:01:02:03" "08-00-2b-01-02-03"
                      "08002b:010203" "08002b-010203"
                      "0800.2b01.0203" "0800-2b01-0203"
                      "08002b010203"
                      ;; Case-insensitive, and surrounding space is trimmed.
                      "08:00:2B:01:02:03" "  08:00:2b:01:02:03  "]]
      (is (= "08:00:2b:01:02:03" (one c (str "select '" spelling "'::macaddr")))
          spelling))))

(deftest macaddr-refuses-what-postgresql-refuses
  (with-open [c (jdbc)]
    (doseq [bad ["08:00:2b:01:02"            ; five groups
                 "08:00:2b:01:02:03:04"      ; seven
                 "08:00:2b:01:02:0g"         ; not hex
                 "08:00:2b-01:02:03"         ; separators mixed
                 "garbage"]]
      (is (= "22P02" (state-of c (str "select '" bad "'::macaddr"))) bad))))

(deftest macaddr8-is-a-scanner-not-a-pattern-list
  (with-open [c (jdbc)]
    (testing "eight groups, any even width, one consistent separator"
      (doseq [spelling ["08:00:2b:01:02:03:04:05" "08-00-2b-01-02-03-04-05"
                        "08002b:0102030405" "0800.2b01.0203.0405"
                        "08002b0102030405"]]
        (is (= "08:00:2b:01:02:03:04:05"
               (one c (str "select '" spelling "'::macaddr8")))
            spelling)))
    (testing "six groups is an EUI-48, widened by inserting ff:fe after
              the third byte -- not left-padded"
      (is (= "08:00:2b:ff:fe:01:02:03" (one c "select '08:00:2b:01:02:03'::macaddr8")))
      (is (= "08:00:2b:ff:fe:01:02:03" (one c "select '08002b010203'::macaddr8"))))
    (testing "seven groups is neither"
      (is (= "22P02" (state-of c "select '08:00:2b:01:02:03:04'::macaddr8"))))))

(deftest trunc-keeps-the-oui
  ;; One name, two functions: `trunc(macaddr)` zeroes the last THREE
  ;; bytes and `trunc(macaddr8)` the last FIVE -- everything after the
  ;; 24-bit OUI in both. PostgreSQL tells them apart by type; we have
  ;; only the value, so the canonical form is the discriminator, and a
  ;; numeric `trunc` is unaffected because a number has no colon in it.
  (with-open [c (jdbc)]
    (is (= "08:00:2b:00:00:00" (one c "select trunc('08:00:2b:01:02:03'::macaddr)")))
    (is (= "08:00:2b:00:00:00:00:00"
           (one c "select trunc('08:00:2b:01:02:03:04:05'::macaddr8)")))
    (testing "numeric trunc still truncates"
      (is (= "3" (one c "select trunc(3.7)")))
      (is (= "3.70" (one c "select trunc(3.7049, 2)"))))))

(deftest the-conversions-between-the-two
  (with-open [c (jdbc)]
    (is (= "08:00:2b:ff:fe:01:02:03" (one c "select '08:00:2b:01:02:03'::macaddr::macaddr8")))
    (is (= "08:00:2b:03:04:05" (one c "select '08:00:2b:ff:fe:03:04:05'::macaddr8::macaddr")))
    (testing "narrowing is only defined when ff:fe are there, because
              any other address would lose information"
      (is (= "22003" (state-of c "select '08:00:2b:01:02:03:04:05'::macaddr8::macaddr"))))))

(deftest set7bit-and-the-bitwise-operators
  (with-open [c (jdbc)]
    (is (= "02:00:2b:01:02:03:04:05"
           (one c "select macaddr8_set7bit('00:00:2b:01:02:03:04:05'::macaddr8)")))
    (is (= "f7:ff:d4:fe:fd:fc" (one c "select ~ '08:00:2b:01:02:03'::macaddr")))
    (is (= "08:00:2b:00:00:00"
           (one c "select '08:00:2b:01:02:03'::macaddr & 'ff:ff:ff:00:00:00'::macaddr")))
    (is (= "ff:ff:ff:01:02:03"
           (one c "select '08:00:2b:01:02:03'::macaddr | 'ff:ff:ff:00:00:00'::macaddr")))
    (testing "integer bitwise is untouched"
      (is (= "8" (one c "select 12 & 9")))
      (is (= "13" (one c "select 12 | 9")))
      (is (= "-13" (one c "select ~ 12"))))))

(deftest a-column-round-trips-and-reports-its-own-type
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE ma (a macaddr, b macaddr8)")
    (exec! c "INSERT INTO ma VALUES ('0800.2b01.0203', '08002b0102030405')")
    (testing "two spellings of one address are ONE value, because INSERT
              coerces too -- the cast path alone left `'garbage'` in a
              macaddr column succeeding"
      (is (= "22P02" (state-of c "INSERT INTO ma VALUES ('garbage', NULL)")))
      (is (= "08:00:2b:01:02:03" (one c "SELECT a FROM ma")))
      (is (= "08:00:2b:01:02:03:04:05" (one c "SELECT b FROM ma"))))
    (testing "and the catalog reports macaddr, not the storage type"
      (is (= "macaddr" (one c (str "SELECT format_type(atttypid, atttypmod) "
                                   "FROM pg_attribute WHERE attrelid = 'ma'::regclass "
                                   "AND attname = 'a'"))))
      (is (= "macaddr8" (one c (str "SELECT format_type(atttypid, atttypmod) "
                                    "FROM pg_attribute WHERE attrelid = 'ma'::regclass "
                                    "AND attname = 'b'")))))))

(deftest text-order-is-byte-order
  ;; The reason canonical text is enough: no separate comparator.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE mo (a macaddr)")
    (exec! c (str "INSERT INTO mo VALUES ('ff:ff:ff:ff:ff:ff'), ('08:00:2b:01:02:04'), "
                  "('08:00:2b:01:02:03'), ('00:00:00:00:00:00')"))
    (is (= "00:00:00:00:00:00" (one c "SELECT a FROM mo ORDER BY a LIMIT 1")))
    (is (= "ff:ff:ff:ff:ff:ff" (one c "SELECT a FROM mo ORDER BY a DESC LIMIT 1")))
    (is (= "08:00:2b:01:02:03"
           (one c "SELECT a FROM mo WHERE a > '00:00:00:00:00:00' ORDER BY a LIMIT 1")))
    (is (= "t" (one c "SELECT '08:00:2b:01:02:03'::macaddr = '08-00-2b-01-02-03'::macaddr")))
    (testing "there is no min/max aggregate for macaddr -- PostgreSQL has
              none either, and says so in the same words"
      (is (= "42883" (state-of c "SELECT min(a) FROM mo"))))))

(deftest arrays-of-the-new-types-still-work
  ;; A REGRESSION these commits introduced, found by review. Giving
  ;; these types a `pg-type-hint` did not make their arrays work and did
  ;; not leave them alone: `sql-name->elem-kw` had no entry, so
  ;; `array-spec` came out nil, the column took the SCALAR hint, and
  ;; INSERT ran `macaddr_in` over the whole `{...}` literal. Before the
  ;; types existed the same column behaved as `text[]` and ACCEPTED
  ;; values, so this turned working DDL into a hard error -- and an
  ;; ARRAY value reached the input function as its Java toString,
  ;; putting `datahike.pg.arrays.PgArray@f30a7cfd` in a user-facing
  ;; message.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE ar (id int, m macaddr[], b box[], p point[])")
    (exec! c (str "INSERT INTO ar VALUES "
                  "(1, '{08002B010203,0800.2b01.0204}', '{\"(1,2),(3,4)\"}', "
                  "'{\"(1,2)\"}')"))
    (testing "elements are read by the ELEMENT type's input function, as
              array_in does -- so they canonicalise and garbage is refused"
      (is (= "{08:00:2b:01:02:03,08:00:2b:01:02:04}" (one c "SELECT m FROM ar")))
      (is (= "{(3,4),(1,2)}" (one c "SELECT b FROM ar")))
      (is (= "22P02" (state-of c "SELECT '{garbage}'::macaddr[]"))))
    (testing "the column reports its own array type, not text[]"
      (is (= "macaddr[]" (one c (str "SELECT format_type(atttypid, atttypmod) "
                                     "FROM pg_attribute WHERE attrelid = 'ar'::regclass "
                                     "AND attname = 'm'"))))
      (is (= "box[]" (one c (str "SELECT format_type(atttypid, atttypmod) "
                                 "FROM pg_attribute WHERE attrelid = 'ar'::regclass "
                                 "AND attname = 'b'")))))))

(deftest box-arrays-are-delimited-by-semicolons
  ;; `box` is the only type in all of PostgreSQL whose pg_type.typdelim
  ;; is not a comma, and it has to be: a box PRINTS as `(3,4),(1,2)`,
  ;; which already contains two commas. With a comma delimiter
  ;; PostgreSQL's own `box[]` output read back as four elements here,
  ;; and ours could not be read by PostgreSQL at all.
  (with-open [c (jdbc)]
    (is (= "{(3,4),(1,2)}" (one c "SELECT '{(3,4),(1,2)}'::box[]")))
    (is (= "1" (one c "SELECT array_length('{(3,4),(1,2)}'::box[], 1)")))
    (is (= "2" (one c "SELECT array_length('{(3,4),(1,2);(9,9),(8,8)}'::box[], 1)")))
    (is (= "(9,9),(8,8)" (one c "SELECT ('{(3,4),(1,2);(9,9),(8,8)}'::box[])[2]")))
    (testing "our own output is what we read back, and what PostgreSQL emits"
      (is (= "{(3,4),(1,2);(7,8),(5,6)}"
             (one c "SELECT ARRAY['(1,2),(3,4)'::box, '(5,6),(7,8)'::box]"))))
    (testing "the comma types are untouched"
      (is (= "{a,\"b,c\"}" (one c "SELECT '{a,\"b,c\"}'::text[]")))
      (is (= "{{1,2},{3,4}}" (one c "SELECT '{{1,2},{3,4}}'::int[]"))))))
