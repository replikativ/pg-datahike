(ns datahike.regress.repl
  "Drive PostgreSQL's own regression SQL against both servers from a REPL.

   `bb pg-regress` runs a whole file through `pg_regress` and takes ~40
   seconds; finding out WHY a file diverges that way is a slow loop. This
   replays a file statement by statement against the PostgreSQL oracle and
   against pg-datahike, and stops at the first statement where they
   disagree -- which is the only statement that matters, because 97% of
   every file's output comes after its first divergence.

     (first-divergence \"timestamp\")
     (compare-sql \"SELECT '2000-04-01'::date\")
     (replay \"timestamp\" 40)        ; the first 40 statements, all diffs"
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.sql Connection DriverManager SQLException Statement ResultSet]))

;; Both servers are the developer's own: the oracle is the PostgreSQL 17
;; `bb pg-oracle` starts, the target the `bb pg-server` on 15432. Override
;; through the environment to point at others.
(def ^:dynamic *oracle-tcp*
  (or (System/getenv "PG_ORACLE_URL")
      "jdbc:postgresql://127.0.0.1:15998/postgres?user=pgtest&password=pgtest&sslmode=disable"))
(def ^:dynamic *target*
  (or (System/getenv "PG_TARGET_URL")
      "jdbc:postgresql://127.0.0.1:15432/datahike?user=datahike&password=datahike&sslmode=disable&binaryTransfer=false"))

(def regress-sql-dir
  (or (System/getenv "PG_REGRESS_SQL_DIR")
      (str (System/getProperty "user.home") "/Development/postgres/src/test/regress/sql")))

;; ── splitting a regress file into statements ───────────────────────────

(defn split-statements
  "The statements of a psql script: `;` terminates at top level, `$tag$`
   bodies and quoted strings swallow it, `--` runs to end of line, and a
   line starting with a backslash is a psql metacommand of its own."
  [^String sql]
  (let [n (count sql)]
    (loop [i 0, start 0, out [], quote nil, dollar nil]
      (if (>= i n)
        (let [last (str/trim (subs sql start))]
          (cond-> out (seq last) (conj last)))
        (let [c (.charAt sql i)]
          (cond
            dollar
            (if (and (= c \$) (str/starts-with? (subs sql i (min n (+ i (count dollar)))) dollar))
              (recur (+ i (count dollar)) start out nil nil)
              (recur (inc i) start out nil dollar))

            quote
            (recur (inc i) start out (when-not (= c quote) quote) nil)

            (and (= c \-) (< (inc i) n) (= \- (.charAt sql (inc i))))
            (let [eol (or (str/index-of sql "\n" i) n)] (recur eol start out nil nil))

            ;; A `/* … */` block comment, which may span lines and nest.
            ;; Skipping only `--` left the tail of one in the next
            ;; statement, and the server then reported a lexical error
            ;; that belonged to the harness.
            (and (= c \/) (< (inc i) n) (= \* (.charAt sql (inc i))))
            (let [close (loop [j (+ i 2) depth 1]
                          (cond
                            (>= j (dec n)) n
                            (and (= \/ (.charAt sql j)) (= \* (.charAt sql (inc j))))
                            (recur (+ j 2) (inc depth))
                            (and (= \* (.charAt sql j)) (= \/ (.charAt sql (inc j))))
                            (if (= 1 depth) (+ j 2) (recur (+ j 2) (dec depth)))
                            :else (recur (inc j) depth)))]
              (recur close start out nil nil))

            (= c \$)
            (if-let [m (re-find #"^\$[A-Za-z_0-9]*\$" (subs sql i (min n (+ i 32))))]
              (recur (+ i (count m)) start out nil m)
              (recur (inc i) start out nil nil))

            (or (= c \') (= c \")) (recur (inc i) start out c nil)

            (= c \;)
            (let [stmt (str/trim (subs sql start (inc i)))]
              (recur (inc i) (inc i) (cond-> out (seq stmt) (conj stmt)) nil nil))

            :else (recur (inc i) start out nil nil)))))))

(defn- strip-meta-and-comments
  "Drop psql metacommands and leading comment lines from a split
   statement. A `\\getenv` line sits INSIDE the text that precedes the
   first `;`, so dropping statements that merely start with a backslash
   leaves it in -- and the server then sees a lexical error that has
   nothing to do with the file."
  [stmt]
  (let [stmt (str/replace stmt #"(?s)/\*.*?\*/" "")
        kept (->> (str/split-lines stmt)
                  (remove #(str/starts-with? (str/trim %) "\\"))
                  (remove #(str/starts-with? (str/trim %) "--"))
                  (str/join "\n")
                  str/trim)]
    (when (and (seq kept) (not= kept ";")) kept)))

(defn statements
  "The statements of a named regression file: psql metacommands and
   comment lines removed, since they are the client's, not the server's."
  [test-name]
  (->> (slurp (io/file regress-sql-dir (str test-name ".sql")))
       split-statements
       (keep strip-meta-and-comments)))

;; ── running one statement on both sides ────────────────────────────────

(defn- run-one [^Connection c ^String sql]
  (with-open [st (.createStatement c)]
    (try
      (if (.execute st sql)
        (with-open [rs (.getResultSet st)]
          (let [md (.getMetaData rs)
                n (.getColumnCount md)]
            {:cols (mapv #(.getColumnName md (int %)) (range 1 (inc n)))
             :rows (loop [acc []]
                     (if (.next rs)
                       (recur (conj acc (mapv #(.getString rs (int %)) (range 1 (inc n)))))
                       acc))}))
        {:updated (.getUpdateCount st)})
      (catch SQLException e
        {:error (first (str/split-lines (.getMessage e))) :sqlstate (.getSQLState e)}))))

(defn- open ^Connection [url] (DriverManager/getConnection url))

(defn compare-sql
  "Run one statement on the oracle and on pg-datahike; `:same?` tells
   whether they agree. Opens a connection per call -- fine for a single
   probe, and `replay` reuses one."
  [sql]
  (with-open [a (open *oracle-tcp*) b (open *target*)]
    (let [pg (run-one a sql) we (run-one b sql)]
      {:sql sql :pg pg :we we :same? (= pg we)})))

(defn replay
  "Replay a regression file on both sides, in one session each, and
   return every statement where they disagree (at most `limit`). Both
   sides see the same statements in the same order, so a divergence is
   attributable to the statement it appears on."
  ([test-name] (replay test-name 10))
  ([test-name limit]
   (with-open [a (open *oracle-tcp*) b (open *target*)]
     (let [stmts (statements test-name)]
       (loop [[s & more] stmts, i 0, out []]
         (if (or (nil? s) (>= (count out) limit))
           {:test test-name :statements (count stmts) :checked i :diffs out}
           (let [pg (run-one a s) we (run-one b s)]
             (recur more (inc i)
                    (cond-> out
                      (not= pg we) (conj {:n i :sql s :pg pg :we we}))))))))))

(defn first-divergence
  "The first statement of a regression file where we disagree with
   PostgreSQL -- the one worth fixing, since everything after it is
   downstream."
  [test-name]
  (let [{:keys [diffs statements checked]} (replay test-name 1)]
    (if-let [d (first diffs)]
      (assoc d :statements statements :position (format "%d of %d" (:n d) statements))
      {:test test-name :statements statements :checked checked :same? true})))
