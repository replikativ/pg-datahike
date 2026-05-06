(ns datahike.test.sqllogictest-runner
  "SQLLogicTest runner for Datahike's PostgreSQL compatibility layer.

   Parses .test files in the standard sqllogictest format and executes them
   against Datahike's SQL handler. Each .test file gets a fresh in-memory
   database.

   Format:
     statement ok       — DDL/DML that should succeed
     statement error    — SQL that should fail
     query <types> <sort>
     <sql>
     ----
     <expected tab-separated rows>

   Type codes: I = integer, R = real (6 decimals), T = text
   Sort modes: nosort (exact), rowsort (sort rows), valuesort (sort all values)"
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [datahike.api :as d]
            [datahike.pg :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(set! *warn-on-reflection* true)

;; ============================================================================
;; Result formatting — match sqllogictest conventions
;; ============================================================================

(defn- format-value
  "Format a result value according to sqllogictest type codes.
   I = integer, R = real (6 decimal places), T = text."
  [v type-char]
  (cond
    (nil? v)             "NULL"
    (= :__null__ v)      "NULL"

    (= type-char \I)
    (cond
      (instance? Long v)    (str v)
      (instance? Integer v) (str v)
      (number? v)           (str (long v))
      (string? v)           (try (str (Long/parseLong v)) (catch Exception _ v))
      :else                 (str v))

    (= type-char \R)
    (cond
      (instance? clojure.lang.Ratio v)
      (format "%.6f" (double v))
      (instance? Double v)
      (format "%.6f" (double v))
      (instance? Float v)
      (format "%.6f" (double v))
      (number? v)
      (format "%.6f" (double v))
      (string? v)
      (try (format "%.6f" (Double/parseDouble v)) (catch Exception _ v))
      :else (str v))

    :else ;; T = text
    (cond
      (= :__null__ v) "NULL"
      (keyword? v)    (if-let [ns (namespace v)]
                        (str ns "/" (name v))
                        (name v))
      (boolean? v)    (if v "t" "f")
      :else           (str v))))

(defn- format-result-row
  "Format a single result row as a tab-separated string."
  [row type-str]
  (let [values (if (sequential? row) (vec row) [row])]
    (str/join "\t"
              (map-indexed
               (fn [i v]
                 (let [tc (if (< i (count type-str))
                            (.charAt ^String type-str i)
                            \T)]
                   (format-value v tc)))
               values))))

(defn- format-results
  "Format query results for comparison with expected output."
  [^PgWireServer$QueryResult result type-str sort-mode]
  (when-not (.error result)
    (let [rows (.rows result)
          lines (mapv (fn [^"[Ljava.lang.String;" row]
                        (str/join "\t"
                                  (map-indexed
                                   (fn [i v]
                                     (let [tc (if (< i (count type-str))
                                                (.charAt ^String type-str i)
                                                \T)]
                                       (if (nil? v)
                                         "NULL"
                                         (format-value v tc))))
                                   (vec row))))
                      rows)]
      (case sort-mode
        "rowsort"  (vec (sort lines))
        "valuesort" (vec (sort (mapcat #(str/split % #"\t") lines)))
        ;; nosort or default: preserve order
        (vec lines)))))

;; ============================================================================
;; Test file parser
;; ============================================================================

(defn- parse-test-file
  "Parse a .test file into a sequence of test records.
   Returns [{:type :statement|:query, :expect :ok|:error,
             :sql str, :types str, :sort str, :expected [str ...]}]"
  [^String content]
  (let [lines (str/split-lines content)]
    (loop [i 0
           records []]
      (if (>= i (count lines))
        records
        (let [line (str/trim (nth lines i))]
          (cond
            ;; Skip comments and blank lines
            (or (str/blank? line) (str/starts-with? line "#"))
            (recur (inc i) records)

            ;; statement ok / statement error
            (str/starts-with? line "statement")
            (let [expect (if (str/includes? line "error") :error :ok)
                  ;; Next non-blank, non-comment line is the SQL
                  sql-lines (loop [j (inc i) acc []]
                              (if (or (>= j (count lines))
                                      (str/blank? (nth lines j))
                                      (str/starts-with? (str/trim (nth lines j)) "#")
                                      (str/starts-with? (str/trim (nth lines j)) "statement")
                                      (str/starts-with? (str/trim (nth lines j)) "query"))
                                [acc j]
                                (recur (inc j) (conj acc (nth lines j)))))
                  [sql-parts next-i] sql-lines
                  sql (str/trim (str/join " " sql-parts))]
              (recur (long next-i) (conj records {:type :statement :expect expect :sql sql})))

            ;; query <types> [sort]
            (str/starts-with? line "query")
            (let [parts (str/split line #"\s+")
                  types (if (> (count parts) 1) (nth parts 1) "T")
                  sort-mode (if (> (count parts) 2) (nth parts 2) "nosort")
                  ;; Next line is SQL (may span multiple lines until ----)
                  sql-lines (loop [j (inc i) acc []]
                              (if (or (>= j (count lines))
                                      (= (str/trim (nth lines j)) "----"))
                                [acc (inc j)] ;; skip the ---- line
                                (recur (inc j) (conj acc (nth lines j)))))
                  [sql-parts sep-i] sql-lines
                  sql (str/trim (str/join " " sql-parts))
                  ;; Expected results after ----
                  expected (loop [j sep-i acc []]
                             (if (or (>= j (count lines))
                                     (str/blank? (nth lines j)))
                               [acc j]
                               (recur (inc j) (conj acc (nth lines j)))))
                  [exp-lines next-i] expected]
              (recur (long next-i)
                     (conj records {:type :query :types types :sort sort-mode
                                    :sql sql :expected (vec exp-lines)})))

            ;; Unknown line — skip
            :else
            (recur (inc i) records)))))))

;; ============================================================================
;; Test execution
;; ============================================================================

(defn- execute-record
  "Execute a single test record against a handler. Returns nil on success,
   error message string on failure."
  [^datahike.pg.PgWireServer$QueryHandler handler record]
  (let [{:keys [type expect sql types expected] sort-mode :sort} record]
    (case type
      :statement
      (let [^PgWireServer$QueryResult result (.execute handler sql)]
        (case expect
          :ok
          (when (.error result)
            (str "Expected 'statement ok' but got error: " (.error result)
                 "\n  SQL: " sql))
          :error
          (when-not (.error result)
            (str "Expected 'statement error' but statement succeeded"
                 "\n  SQL: " sql))))

      :query
      (try
        (let [^PgWireServer$QueryResult result (.execute handler sql)]
          (if (.error result)
            (str "Query error: " (.error result) "\n  SQL: " sql)
            (let [actual (format-results result types sort-mode)
                  exp-sorted (case sort-mode
                               "rowsort" (vec (sort expected))
                               "valuesort" (vec (sort (mapcat #(str/split % #"\t") expected)))
                               (vec expected))]
              (when (not= actual exp-sorted)
                (str "Result mismatch for: " sql
                     "\n  Expected (" (count exp-sorted) " rows):"
                     (str/join "\n    " (cons "" exp-sorted))
                     "\n  Actual (" (count actual) " rows):"
                     (str/join "\n    " (cons "" actual)))))))
        (catch Exception e
          (str "Exception executing query: " (.getMessage e)
               "\n  SQL: " sql))))))

(defn run-test-file
  "Run all tests in a .test file against a fresh Datahike database.
   Returns {:passed N :failed N :errors [...]}."
  [^String path]
  (let [content (slurp path)
        records (parse-test-file content)
        cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? true}
        _ (d/create-database cfg)
        conn (d/connect cfg)
        handler (pg/make-query-handler conn)]
    (try
      (let [results (reduce
                     (fn [{:keys [passed failed errors] :as acc} record]
                       (let [msg (execute-record handler record)]
                         (if msg
                           {:passed passed :failed (inc failed) :errors (conj errors msg)}
                           {:passed (inc passed) :failed failed :errors errors})))
                     {:passed 0 :failed 0 :errors []}
                     records)]
        results)
      (finally
        (d/release conn)
        (d/delete-database cfg)))))

;; ============================================================================
;; Clojure test integration
;; ============================================================================

(deftest sqllogictest-suite
  (let [test-dir (io/file "test/sqllogictest")]
    (when (.isDirectory test-dir)
      (doseq [^java.io.File f (sort-by #(.getName ^java.io.File %)
                                       (filter #(.endsWith (.getName ^java.io.File %) ".test")
                                               (file-seq test-dir)))]
        (testing (.getName f)
          (let [{:keys [passed failed errors]} (run-test-file (.getPath f))]
            (doseq [err errors]
              (is (nil? err) err))
            (when (zero? failed)
              (is true (str (.getName f) ": " passed " passed")))))))))
