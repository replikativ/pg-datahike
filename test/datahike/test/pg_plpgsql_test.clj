(ns datahike.test.pg-plpgsql-test
  "plpgsql.

   `pl_gram.y` is the specification for the syntax, and what that
   grammar does -- and what `datahike.pg.plpgsql.parse` does -- is parse
   the statement STRUCTURE and hand every embedded expression and query
   to the SQL layer as source. plpgsql has no expression grammar of its
   own: `IF a > b THEN` finds the THEN and gives `a > b` to the SQL
   parser.

   The evaluator has two primitives -- evaluate an expression, run a
   statement -- and both go through the handler that reached it, which
   is what SPI is for PostgreSQL: the nested statement shares the
   transaction, the temporary tables and the session.

   Unlike a `LANGUAGE sql` function, a plpgsql one cannot be inlined:
   it is imperative, so there is no expression to substitute. It is
   CALLED, once per row, exactly as in PostgreSQL.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.plpgsql.parse :as pl]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(defn- fresh-handler []
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}
        _ (d/create-database cfg)
        conn (d/connect cfg)]
    {:conn conn :cfg cfg :handler (pg/make-query-handler conn {})}))

(defn- exec ^PgWireServer$QueryResult [{:keys [^PgWireServer$QueryHandler handler]} sql]
  (.execute handler sql))

(defn- rows [^PgWireServer$QueryResult r]
  (when-not (.error r) (mapv vec (.rows r))))

(defn- err [^PgWireServer$QueryResult r] (some-> (.error r) str))

(defn- ok! [h sql]
  (let [r (exec h sql)]
    (is (nil? (.error r)) (str sql " => " (err r)))
    r))

(defn- release! [{:keys [conn cfg]}]
  (d/release conn) (d/delete-database cfg))

(defmacro with-h [[sym init] & body]
  `(let [~sym ~init] (try ~@body (finally (release! ~sym)))))

(def ^:private dq "$$")

(defn- defn! [h name args ret body & [attrs]]
  (ok! h (str "CREATE FUNCTION " name "(" args ") RETURNS " ret
              " AS " dq body dq " LANGUAGE plpgsql " (or attrs ""))))

(defn- v1 [h sql] (ffirst (rows (ok! h sql))))

;; ============================================================================
;; The parser
;; ============================================================================

(deftest every-shape-in-the-corpus-parses
  ;; All 258 plpgsql bodies in PostgreSQL's own src/test/regress parse.
  ;; These are the shapes that needed fixing to get there.
  (testing "the four RAISE forms of pl_gram.y"
    (doseq [body ["BEGIN RAISE NOTICE 'x %', 1; END"
                  "BEGIN RAISE division_by_zero USING detail = 'd'; END"
                  "BEGIN RAISE sqlstate '1234F'; END"
                  "BEGIN RAISE EXCEPTION USING message = 'a' || 'b'; END"
                  "BEGIN RAISE; END"]]
      (is (map? (pl/parse-body body)) body)))
  (testing "a compiler directive before the block"
    (is (map? (pl/parse-body "#variable_conflict use_column\ndeclare r int; begin return r; end")))
    (is (map? (pl/parse-body "#print_strict_params on\ndeclare x int; begin return x; end"))))
  (testing "SQLSTATE as an exception condition"
    (is (map? (pl/parse-body
               "begin null; exception when sqlstate 'U9999' then null; end"))))
  (testing "a keyword inside a string or a comment is not a keyword"
    (let [ast (pl/parse-body "BEGIN RAISE NOTICE 'END IF; LOOP'; -- END\n RETURN 1; END")]
      (is (= [:raise :return] (mapv :node (:body ast)))))))

(deftest a-positional-parameter-is-renamed
  ;; `$n` cannot stay `$n`: the translator rewrites LITERALS to `$n`
  ;; placeholders so a plan can be cached across values, so a body's own
  ;; `$1` becomes indistinguishable from a `0` the rewriter lifted --
  ;; `SELECT 0` came back as the function's first argument.
  (let [ast (pl/parse-body "BEGIN RETURN $1 + $2; END")]
    (is (= (str "__plpgsql_arg_1 + __plpgsql_arg_2")
           (:expr (first (:body ast)))))))

(deftest a-body-that-cannot-run-is-named-at-create-time
  (with-h [h (fresh-handler)]
    (doseq [[body construct]
            [["BEGIN EXECUTE 'SELECT 1'; END" "execute"]
             ["BEGIN NULL; EXCEPTION WHEN others THEN NULL; END" "exception-handler"]
             ["DECLARE c CURSOR FOR SELECT 1; BEGIN NULL; END" "cursor-declaration"]
             ["BEGIN GET DIAGNOSTICS x = ROW_COUNT; END" "get-diagnostics"]]]
      (let [r (exec h (str "CREATE FUNCTION nope() RETURNS int AS " dq body dq
                           " LANGUAGE plpgsql"))]
        (is (some? (.error r)) body)
        ;; Named, so it is clear what is missing -- and refused at
        ;; DEFINITION time, not as a wrong answer at the call.
        (is (str/includes? (err r) construct) (str body " => " (err r)))))))

;; ============================================================================
;; Running
;; ============================================================================

(deftest arguments-and-return
  (with-h [h (fresh-handler)]
    (defn! h "add2" "a int, b int" "int" "BEGIN RETURN a + b; END")
    (is (= "5" (v1 h "SELECT add2(2,3)")))
    (testing "positionally, for an unnamed parameter"
      (defn! h "mul" "int, int" "int" "BEGIN RETURN $1 * $2; END")
      (is (= "42" (v1 h "SELECT mul(6,7)"))))
    (testing "a literal in the body is still a literal"
      ;; The regression that motivated renaming `$n`: `SELECT 0` inside
      ;; a body answered the first argument.
      (defn! h "zero" "a int" "int" "DECLARE s int := 0; BEGIN RETURN s; END")
      (is (= "0" (v1 h "SELECT zero(4)"))))
    (testing "RETURNS void is the empty string, not NULL"
      (defn! h "nothing" "" "void" "BEGIN RETURN; END")
      (is (= "" (v1 h "SELECT nothing()"))))))

(deftest variables-keep-their-declared-type
  ;; A statement gives its result back as TEXT. Without coercing on
  ;; assignment, `t int := 0` held the string "0" and `t + 1` was
  ;; `'0' + 1`.
  (with-h [h (fresh-handler)]
    (defn! h "acc" "a int" "int"
      "DECLARE s int := 0; BEGIN WHILE a > 0 LOOP s := s + a; a := a - 1; END LOOP; RETURN s; END")
    (is (= "10" (v1 h "SELECT acc(4)")))))

(deftest control-flow
  (with-h [h (fresh-handler)]
    (testing "IF / ELSIF / ELSE"
      (defn! h "size" "a int" "text"
        "BEGIN IF a > 10 THEN RETURN 'big'; ELSIF a = 10 THEN RETURN 'ten'; ELSE RETURN 'small'; END IF; END")
      (is (= [["small" "ten" "big"]] (rows (ok! h "SELECT size(5), size(10), size(11)")))))
    (testing "an integer FOR loop"
      (defn! h "tri" "n int" "int"
        "DECLARE t int := 0; BEGIN FOR i IN 1 .. n LOOP t := t + i; END LOOP; RETURN t; END")
      (is (= "55" (v1 h "SELECT tri(10)"))))
    (testing "REVERSE and BY"
      (defn! h "rev" "" "int"
        "DECLARE t int := 0; BEGIN FOR i IN REVERSE 10 .. 1 BY 3 LOOP t := t + i; END LOOP; RETURN t; END")
      (is (= "22" (v1 h "SELECT rev()"))))
    (testing "LOOP with EXIT WHEN"
      (defn! h "upto" "n int" "int"
        "DECLARE i int := 0; BEGIN LOOP EXIT WHEN i >= n; i := i + 1; END LOOP; RETURN i; END")
      (is (= "3" (v1 h "SELECT upto(3)"))))
    (testing "CONTINUE"
      (defn! h "evens" "n int" "int"
        (str "DECLARE t int := 0; BEGIN FOR i IN 1 .. n LOOP "
             "CONTINUE WHEN i % 2 = 1; t := t + i; END LOOP; RETURN t; END"))
      (is (= "6" (v1 h "SELECT evens(4)"))))
    (testing "the CASE statement"
      (defn! h "named" "a int" "text"
        "BEGIN CASE a WHEN 1 THEN RETURN 'one'; WHEN 2 THEN RETURN 'two'; ELSE RETURN '?'; END CASE; END")
      (is (= [["one" "two" "?"]] (rows (ok! h "SELECT named(1), named(2), named(9)")))))))

(deftest statements-against-the-database
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int, s text)")
    (ok! h "INSERT INTO t VALUES (1,'a'),(2,'b'),(3,'c')")
    (testing "SELECT … INTO"
      (defn! h "howmany" "" "int"
        "DECLARE c int; BEGIN SELECT count(*) INTO c FROM t; RETURN c; END")
      (is (= "3" (v1 h "SELECT howmany()"))))
    (testing "a FOR loop over a query"
      (defn! h "total" "" "int"
        "DECLARE t2 int := 0; BEGIN FOR r IN SELECT i FROM t ORDER BY i LOOP t2 := t2 + r.i; END LOOP; RETURN t2; END")
      (is (= "6" (v1 h "SELECT total()"))))
    (testing "a write, in the caller's transaction"
      (defn! h "addrow" "n int" "void" "BEGIN INSERT INTO t VALUES (n, 'z'); END")
      (ok! h "SELECT addrow(9)")
      (is (= [["9" "z"]] (rows (ok! h "SELECT i, s FROM t WHERE i = 9")))))
    (testing "called per row, which is what PostgreSQL does too"
      (defn! h "dbl" "a int" "int" "BEGIN RETURN a * 2; END")
      (is (= [["1" "2"] ["2" "4"] ["3" "6"]]
             (rows (ok! h "SELECT i, dbl(i) FROM t WHERE i < 4 ORDER BY i")))))))

(deftest raise
  (with-h [h (fresh-handler)]
    (testing "RAISE EXCEPTION aborts with its message and SQLSTATE"
      (defn! h "boom" "" "int" "BEGIN RAISE EXCEPTION 'nope %', 'here'; END")
      (let [r (exec h "SELECT boom()")]
        ;; The handler carries the message; the wire layer is what
        ;; prefixes "ERROR:".
        (is (= "nope here" (err r)))))
    (testing "a condition name carries its SQLSTATE"
      (defn! h "divzero" "" "int" "BEGIN RAISE division_by_zero; END")
      (is (some? (.error (exec h "SELECT divzero()")))))
    (testing "%% is a literal per cent, and a NULL argument prints <NULL>"
      (defn! h "pct" "" "int" "BEGIN RAISE EXCEPTION '100%% of %', NULL; END")
      (is (str/includes? (err (exec h "SELECT pct()")) "100% of <NULL>")))
    (testing "too few arguments for the format is an error, not a silent gap"
      (defn! h "short" "" "int" "BEGIN RAISE EXCEPTION 'a % b %', 1; END")
      (is (str/includes? (err (exec h "SELECT short()")) "too few parameters")))))

(deftest strict-is-honoured
  (with-h [h (fresh-handler)]
    (defn! h "sconst" "x int" "text" "BEGIN RETURN 'const'; END" "STRICT")
    (is (= nil (v1 h "SELECT sconst(NULL)")))
    (is (= "const" (v1 h "SELECT sconst(1)")))))

(deftest recursion-terminates
  (with-h [h (fresh-handler)]
    (defn! h "fact" "n int" "int"
      "BEGIN IF n <= 1 THEN RETURN 1; END IF; RETURN n * fact(n - 1); END")
    (is (= "120" (v1 h "SELECT fact(5)")))
    (testing "and runaway recursion stops rather than exhausting the stack"
      (defn! h "forever" "n int" "int" "BEGIN RETURN forever(n + 1); END")
      (let [r (exec h "SELECT forever(1)")]
        (is (some? (.error r)))
        (is (str/includes? (err r) "stack depth"))))))

(deftest a-value-that-will-not-cast-raises-where-it-is-assigned
  ;; Both boundaries where plpgsql applies a declared type used to
  ;; `(catch Exception _ v)` and hand the untyped TEXT onward. The cast
  ;; failure then surfaced far from its cause, as a JVM message
  ;; ("class java.lang.String cannot be cast to class
  ;; java.lang.Number") from whatever arithmetic touched the value
  ;; next. PostgreSQL raises the type's own input error, at the
  ;; assignment and at the RETURN.
  (with-h [h (fresh-handler)]
    (testing "assignment to a declared variable"
      (defn! h "f" "" "int"
        "DECLARE x int; BEGIN x := 'abc'; RETURN x + 1; END")
      (is (= "invalid input syntax for type integer: \"abc\""
             (err (exec h "SELECT f()")))))
    (testing "RETURN, where the caller's expression would be the victim"
      (defn! h "g" "" "int" "BEGIN RETURN 'xyz'; END")
      (is (= "invalid input syntax for type integer: \"xyz\""
             (err (exec h "SELECT 2 * g()")))))
    (testing "and a value that does cast still passes through both"
      (defn! h "ok2" "" "int"
        "DECLARE x int; BEGIN x := '41'; RETURN x + 1; END")
      (is (= "42" (v1 h "SELECT ok2()"))))))
