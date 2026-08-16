(ns datahike.test.pg-lexical-syntax-test
  "Issue #29 — `SELECT 42#` must be a syntax error.

   PostgreSQL rejects it in the lexer: `#` is an operator character and
   is never part of an unquoted identifier. JSqlParser's lexer is
   laxer, so `42#` parsed as a COLUMN named `42#`, which then resolved
   to nothing and answered zero rows — a malformed statement reported
   as an empty result.

   The check is over `:op` tokens from our own tokeniser rather than
   over the raw text, so a `#` inside a string literal, a dollar-quoted
   string, a quoted identifier or a comment stays untouched.

   `$` gets the same treatment: outside a `$N` placeholder or `$…$`
   quoting it is not valid PG syntax either, and `SELECT 42$` silently
   answered zero rows the same way."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$PgProtocolException
            PgWireServer$QueryResult]))

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

(defn- parse-err
  "[sqlstate message] when Parse rejects `sql`, else nil."
  [sql]
  (try (.parse *handler* sql (int-array 0)) nil
       (catch PgWireServer$PgProtocolException e
         [(.-sqlstate e) (.getMessage e)])))

(defn- parse-err-exec
  "[sqlstate message] from EXECUTING `sql` (not merely parsing it) — the
   Simple Query path is where the placeholder check lives."
  [sql]
  (let [^PgWireServer$QueryResult r (.execute *handler* sql)]
    (when-let [e (.-error r)]
      [(.-sqlstate r) e])))

(defn- rows [sql]
  (let [^PgWireServer$QueryResult r (.execute *handler* sql)]
    (mapv vec (.-rows r))))

(deftest trailing-hash-is-a-syntax-error
  (testing "the exact query from issue #29"
    (is (= "42601" (first (parse-err "SELECT 42#")))))
  (testing "separated, and repeated"
    (is (= "42601" (first (parse-err "SELECT 42 #"))))
    (is (= "42601" (first (parse-err "SELECT 1 ###"))))))

(deftest trailing-dollar-is-a-syntax-error
  (is (= "42601" (first (parse-err "SELECT 42$")))))

(deftest hash-in-an-operator-position-is-rejected-not-mis-parsed
  (testing "we implement no `#` operator, so it is a syntax error rather
            than a column named `a#b`"
    (is (= "42601" (first (parse-err "SELECT a#b"))))
    (is (= "42601" (first (parse-err "SELECT 5 # 3"))))))

(deftest error-message-names-the-offending-token
  (let [[_ msg] (parse-err "SELECT 42#")]
    (is (re-find #"syntax error at or near" msg))
    (is (re-find #"#" msg))))

;; ---------------------------------------------------------------------------
;; The check must not fire where `#` is legitimate text
;; ---------------------------------------------------------------------------

(deftest hash-inside-a-string-literal-is-fine
  (is (nil? (parse-err "SELECT '#'")))
  (is (= [["#"]] (rows "SELECT '#'")))
  (is (= [["a#b"]] (rows "SELECT 'a#b'"))))

(deftest hash-inside-a-comment-is-fine
  (is (nil? (parse-err "SELECT 1 -- x#y")))
  (is (nil? (parse-err "SELECT 1 /* x#y */"))))

(deftest hash-inside-a-quoted-identifier-is-fine
  (let [^PgWireServer$QueryResult r (.execute *handler* "SELECT 1 AS \"a#b\"")]
    (is (= ["a#b"] (vec (.-columnNames r))))))

(deftest dollar-placeholders-are-fine
  (testing "$N is a parameter, not a stray dollar sign"
    (.execute *handler* "CREATE TABLE dp (id int PRIMARY KEY)")
    (is (nil? (parse-err "SELECT id FROM dp WHERE id = $1")))))

(deftest dollar-inside-an-identifier-is-fine
  (testing "PG allows $ in an identifier after the first character"
    (.execute *handler* "CREATE TABLE t$1 (a$b int)")
    (is (nil? (parse-err "SELECT a$b FROM t$1")))
    (is (= [] (rows "SELECT a$b FROM t$1")))))

;; ---------------------------------------------------------------------------
;; `$N` in Simple Query
;; ---------------------------------------------------------------------------

(deftest placeholder-in-simple-query-raises
  (testing "Simple Query has no Bind step, so a `$N` refers to nothing.
            It used to translate anyway and hand the client the internal
            ParamRef record rendered as {\"idx\":1}, as if it were data."
    (is (= ["42P02" "there is no parameter $1"] (parse-err-exec "SELECT $1")))
    (is (= ["42P02" "there is no parameter $1"] (parse-err-exec "SELECT $1 + 1")))
    (is (= ["42P02" "there is no parameter $2"]
           (parse-err-exec "SELECT 1 WHERE 1 = $2"))))
  (testing "a `$` that is not a placeholder is untouched"
    (is (nil? (parse-err-exec "SELECT '$1'")))
    (is (= [["$1"]] (rows "SELECT '$1'"))))
  (testing "PREPARE's template placeholders are legal — they bind at EXECUTE"
    (is (nil? (parse-err-exec "PREPARE pq AS SELECT $1")))))
