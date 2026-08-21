(ns datahike.test.pg-string-datetime-fns-test
  "String, regex and date/time function coverage, and the NULL and
   argument-order bugs found while filling it in.

   Driven by a systematic check rather than one failing query at a time:
   `tools/fncov.clj` reads every pg_catalog overload of a function from the
   ORACLE's own pg_proc, synthesises a call from the argument types, runs it
   on both servers and diffs. That turned \"substring errors\" into a list of
   33 missing functions and 8 wrong ones.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn sd-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"sd" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each sd-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/sd?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- col [^Connection c n sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs (int n)))) acc))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE ft (id int, i int, j int, s text, n numeric, d date)")
  (exec! c (str "INSERT INTO ft VALUES "
                "(1,10,20,'aa',1.50,'2020-01-01'),"
                "(2,NULL,20,'bb',2.25,NULL),"
                "(3,10,NULL,NULL,NULL,'2021-06-15'),"
                "(4,-3,0,'dd',0.00,'2019-12-31'),"
                "(5,0,7,'',10,'2022-02-28')")))

(deftest between-is-kleene-not-strict
  (with-open [c (jdbc)]
    (seed! c)
    ;; `x NOT BETWEEN lo AND hi` was `(or [(< x lo)] [(> x hi)])`, whose two
    ;; branches bind DIFFERENT variable sets the moment lo and hi are
    ;; different columns -- datalog rejects that outright.
    (is (= ["4" "5"] (col c 1 "SELECT id FROM ft WHERE id NOT BETWEEN 0 AND i ORDER BY id")))
    (is (= ["1" "3"] (col c 1 "SELECT id FROM ft WHERE id BETWEEN 0 AND i ORDER BY id")))
    (testing "a NULL bound does not simply make it false -- Kleene AND"
      ;; `1 >= 3` is FALSE, and FALSE AND UNKNOWN is FALSE, so the negation
      ;; is TRUE. Guarding every operand non-NULL first dropped these rows.
      (is (= "f" (one c "SELECT 1 BETWEEN 3 AND NULL")))
      (is (nil? (one c "SELECT 1 BETWEEN 0 AND NULL")) "here it really is UNKNOWN")
      (is (= "t" (one c "SELECT 1 NOT BETWEEN 3 AND NULL")))
      (is (= ["2" "3" "4" "5"]
             (col c 1 "SELECT id FROM ft WHERE 1 NOT BETWEEN id AND n ORDER BY id"))))))

(deftest not-like-over-constants
  (with-open [c (jdbc)]
    (seed! c)
    ;; Same ground-negation problem as `NOT (1 = 1)`: a datalog `not` needs
    ;; variables to NOT-JOIN on, and there are none when both operands are
    ;; literals. Decided at translate time now.
    (is (= [] (col c 1 "SELECT id FROM ft WHERE 'aa' NOT LIKE 'a%' ORDER BY id")))
    (is (= ["1" "2" "3" "4" "5"]
           (col c 1 "SELECT id FROM ft WHERE 'aa' NOT LIKE 'zz' ORDER BY id")))
    (testing "the column forms are unchanged"
      (is (= ["1"] (col c 1 "SELECT id FROM ft WHERE s LIKE 'a%' ORDER BY id")))
      (is (= ["2" "4" "5"] (col c 1 "SELECT id FROM ft WHERE s NOT LIKE 'a%' ORDER BY id"))))))

(deftest sql-keyword-call-forms
  (with-open [c (jdbc)]
    (seed! c)
    ;; `substring(s FROM 1 FOR 2)`, `position('a' IN s)` and TRIM put their
    ;; operands in a NamedExpressionList (or their own AST node) and leave
    ;; .getParameters empty, so they arrived with no arguments at all.
    (is (= ["aa" "bb" nil "dd" ""]
           (col c 2 "SELECT id, substring(s from 1 for 2) AS c FROM ft ORDER BY id")))
    (is (= ["aa" "bb" nil "dd" ""]
           (col c 2 "SELECT id, substring(s, 1, 2) AS c FROM ft ORDER BY id"))
        "the comma form agrees with the keyword form")
    (is (= ["1" "0" nil "0" "0"]
           (col c 2 "SELECT id, position('a' in s) AS c FROM ft ORDER BY id")))
    (is (= ["aa" "bb" nil "dd" ""] (col c 2 "SELECT id, trim(s) AS c FROM ft ORDER BY id")))
    (is (= "aa" (one c "SELECT trim(BOTH 'x' FROM 'xxaaxx')")))
    (is (= "aaxx" (one c "SELECT trim(LEADING 'x' FROM 'xxaaxx')")))
    (is (= "xxaa" (one c "SELECT trim(TRAILING 'x' FROM 'xxaaxx')")))))

(deftest substring-clamps-and-propagates-null
  (with-open [c (jdbc)]
    ;; A raw `subs` threw on the `:__null__` sentinel ("class Keyword cannot
    ;; be cast to String") and on any offset past the end, where PostgreSQL
    ;; simply returns as much as overlaps.
    (is (= "bcd" (one c "SELECT substring('abcdef', 2, 3)")))
    (is (= "a" (one c "SELECT substring('abc', 0, 2)")) "the window starts before the string")
    (is (= "cdef" (one c "SELECT substring('abcdef', 3)")))
    (is (= "" (one c "SELECT substring('abc', 10, 2)")))
    (is (nil? (one c "SELECT substring(NULL::text from 1 for 2)")))))

(deftest position-and-strpos-take-opposite-argument-orders
  (with-open [c (jdbc)]
    ;; `position(sub IN str)` reads sub-first, but gram.y swaps the operands
    ;; before analysis, so the FUNCTION -- and strpos, which shares textpos
    ;; with it -- takes (string, substring). Both pointed at a
    ;; (substring, string) implementation, so strpos answered 0.
    (is (= "3" (one c "SELECT strpos('abcdef', 'cd')")))
    (is (= "3" (one c "SELECT position('cd' in 'abcdef')")))
    (is (= "0" (one c "SELECT strpos('abcdef', 'zz')")))))

(deftest string-functions
  (with-open [c (jdbc)]
    (is (= "97" (one c "SELECT ascii('abc')")))
    (is (= "A" (one c "SELECT chr(65)")))
    (is (= "xy" (one c "SELECT btrim('  xy  ')")))
    (is (= "hi" (one c "SELECT btrim('xxhixx', 'x')")) "the 2-arg form takes a character SET")
    (is (= "hi" (one c "SELECT ltrim('xxhi', 'x')")))
    (is (= "hi" (one c "SELECT rtrim('hixx', 'x')")))
    (is (= "900150983cd24fb0d6963f7d28e17f72" (one c "SELECT md5('abc')")))
    (is (= "t" (one c "SELECT starts_with('abcdef', 'abc')")))
    (is (= "b" (one c "SELECT split_part('a,b,c', ',', 2)")))
    (is (= "c" (one c "SELECT split_part('a,b,c', ',', -1)")) "a negative n counts from the end")
    (is (= "" (one c "SELECT split_part('a,b,c', ',', 9)")))
    (is (= "a2x5" (one c "SELECT translate('12345', '143', 'ax')"))
        "a `from` character with no counterpart in `to` is DELETED")
    (is (= "Thomas" (one c "SELECT overlay('Txxxxas' placing 'hom' from 2 for 4)")))
    (is (= "abc" (one c "SELECT quote_ident('abc')")) "a plain lowercase identifier stays bare")
    (is (= "\"A b\"" (one c "SELECT quote_ident('A b')")))
    (is (= "'O''Hara'" (one c "SELECT quote_literal('O''Hara')")))
    (is (= "NULL" (one c "SELECT quote_nullable(NULL)"))
        "quote_nullable is NOT strict -- rendering the NULL is its whole job")
    (is (= "ff" (one c "SELECT to_hex(255)")))))

(deftest regex-functions
  (with-open [c (jdbc)]
    (testing "regexp_replace replaces only the FIRST match without the g flag"
      (is (= "zCABCAXY" (one c "SELECT regexp_replace('ABCABCAXY', 'A.', 'z')")))
      (is (= "zCzCzY" (one c "SELECT regexp_replace('ABCABCAXY', 'A.', 'z', 'g')")))
      (is (= "zCzCzY" (one c "SELECT regexp_replace('ABCABCAXY', 'a.', 'z', 'gi')"))))
    (testing "capture references are backslash-numbered, not dollar-numbered"
      (is (= "bar foo" (one c "SELECT regexp_replace('foo bar', '(\\w+) (\\w+)', '\\2 \\1')"))))
    (is (= "f" (one c "SELECT regexp_like('ABC', 'b')")))
    (is (= "t" (one c "SELECT regexp_like('ABC', 'b', 'i')")))
    (is (= "3" (one c "SELECT regexp_count('ABCABCAXYaxy', 'A.')")))
    (is (= "4" (one c "SELECT regexp_count('ABCABCAXYaxy', 'A.', 1, 'i')")))
    (is (= "CDE" (one c "SELECT regexp_substr('ABCDEFGHI', 'C..')")))
    (is (= "AB" (one c "SELECT regexp_substr('ABCABCAXY', 'A.', 1, 2)"))
        "the SECOND match, not the second character")
    (is (nil? (one c "SELECT regexp_substr('ABC', 'zz')")))
    (is (= "3" (one c "SELECT regexp_instr('ABCDEFGHI', 'C..')")))
    (is (= "6" (one c "SELECT regexp_instr('ABCDEFGHI', 'C..', 1, 1, 1)"))
        "endoption 1 asks for the position AFTER the match")
    (is (= "0" (one c "SELECT regexp_instr('ABC', 'zz')")))))

(deftest extract-and-date-part
  (with-open [c (jdbc)]
    (seed! c)
    ;; EXTRACT is its own AST node, not a Function, so it never reached the
    ;; function table at all.
    (is (= "2021" (one c "SELECT extract(year from DATE '2021-06-15')")))
    (is (= "6" (one c "SELECT extract(month from DATE '2021-06-15')")))
    (is (= "15" (one c "SELECT extract(day from DATE '2021-06-15')")))
    (is (= "2" (one c "SELECT extract(quarter from DATE '2021-06-15')")))
    (is (= "2" (one c "SELECT extract(dow from DATE '2021-06-15')")) "0 = Sunday, as PG counts")
    (is (= "166" (one c "SELECT extract(doy from DATE '2021-06-15')")))
    (is (= "10" (one c "SELECT extract(hour from TIMESTAMP '2021-06-15 10:20:30')")))
    (is (= "2021" (one c "SELECT date_part('year', DATE '2021-06-15')")))
    (is (nil? (one c "SELECT extract(year from NULL::date)")))
    (is (= ["2020" "2021" "2019" "2022"]
           (remove nil? (col c 2 "SELECT id, extract(year from d) AS c FROM ft ORDER BY id"))))))

(deftest date-trunc
  (with-open [c (jdbc)]
    (seed! c)
    ;; A `date` column arrives as a LocalDate, which the fall-through
    ;; returned UNTRUNCATED -- `date_trunc('month', d)` answered d.
    (is (= "2020-03-01 00:00:00+00" (one c "SELECT date_trunc('month', DATE '2020-03-15')")))
    (is (= "2020-01-01 00:00:00+00" (one c "SELECT date_trunc('year', DATE '2020-03-15')")))
    (testing "and a NULL keeps its row rather than dropping it"
      ;; The implementation returned nil, and a datalog binding that yields
      ;; nil FILTERS THE ROW.
      (is (= 5 (count (col c 1 "SELECT id, date_trunc('month', d) AS c FROM ft ORDER BY id"))))
      (is (nil? (one c "SELECT date_trunc('month', NULL::date)"))))))
