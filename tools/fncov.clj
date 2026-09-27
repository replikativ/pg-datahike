(ns fncov
  "Systematic function-coverage check against the PostgreSQL oracle.

   Rather than compare our function TABLE against a list of names -- which
   understates coverage, since aggregates and many special forms are
   dispatched elsewhere -- this CALLS each documented function on both
   servers and diffs the answer. Signatures come from the oracle's own
   pg_proc, so the argument list is always one PostgreSQL actually accepts."
  (:require [clojure.string :as str])
  (:import [java.sql Connection DriverManager]))

(defn conn ^Connection [port user db]
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" port "/" db
        "?user=" user "&password=x&sslmode=disable&binaryTransfer=false")))

(defn q1 [^Connection c sql]
  (try
    (with-open [st (.createStatement c) rs (.executeQuery st sql)]
      (if (.next rs) [:ok (.getString rs 1)] [:ok nil]))
    (catch Exception e
      [:error (first (str/split-lines (or (.getMessage e) "")))])))

;; One literal per PG type. Chosen so the call is meaningful (a real date,
;; a string with something to find) rather than merely well-typed.
(def lit
  {"text" "'abc'", "varchar" "'abc'", "name" "'abc'", "bpchar" "'abc'",
   "char" "'a'", "int2" "2", "int4" "2", "int8" "2",
   "float4" "2.5", "float8" "2.5", "numeric" "2.5", "bool" "true",
   "date" "DATE '2020-01-15'", "timestamp" "TIMESTAMP '2020-01-15 10:20:30'",
   "timestamptz" "TIMESTAMPTZ '2020-01-15 10:20:30+00'",
   "time" "TIME '10:20:30'", "interval" "INTERVAL '1 day'",
   "bytea" "'\\x6162'::bytea", "oid" "16384", "regclass" "'pg_class'::regclass"})

(defn candidates
  "Every pg_catalog overload of `names` whose arguments we can synthesise,
   lowest arity first so the simplest usable form of each function wins.

   Argument types come from pg_type.typname, NOT
   pg_get_function_identity_arguments -- that renders SQL spellings
   (\"integer\", \"timestamp without time zone\") which no typname table
   matches, and every such function was silently skipped as unbuildable."
  [^Connection oracle names]
  (let [in (str/join "," (map #(str \' % \') names))
        sql (str "SELECT p.proname, p.pronargs, "
                 "  coalesce((SELECT string_agg(t.typname, ',' ORDER BY x.ord) "
                 "            FROM unnest(p.proargtypes) WITH ORDINALITY AS x(oid, ord) "
                 "            JOIN pg_type t ON t.oid = x.oid), '') "
                 "FROM pg_proc p WHERE p.pronamespace='pg_catalog'::regnamespace "
                 "AND p.proname IN (" in ") AND p.prokind='f' "
                 "AND p.proretset=false "
                 "ORDER BY p.proname, p.pronargs")]
    (with-open [st (.createStatement oracle) rs (.executeQuery st sql)]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc [(.getString rs 1) (.getString rs 3)]))
          acc)))))

(defn build-call
  "A callable SQL expression for one overload, or nil when a parameter type
   has no literal we can synthesise."
  [fname typestr]
  (let [types (remove str/blank? (map str/trim (str/split (or typestr "") #",")))]
    (when (every? lit types)
      (str "SELECT " fname "(" (str/join ", " (map lit types)) ")"))))

(defn run [names]
  (with-open [o (conn 15998 "pgtest" "postgres")
              t (conn 15432 "datahike" "datahike")]
    (let [cands (candidates o names)
          ;; EVERY buildable overload, not one per name: the overloads are
          ;; where the divergences hide -- `length(text)` agreed while
          ;; `length(bytea)` counted the hex TEXT and answered 6 for 2 bytes.
          calls (into (sorted-map)
                      (keep (fn [[fname argstr]]
                              (when-let [c (build-call fname argstr)] [c fname])))
                      cands)
          results (for [[sql fname] calls]
                    (let [a (q1 o sql) b (q1 t sql)]
                      {:fn fname :sql sql :pg a :ours b
                       :status (cond
                                 (= a b) :match
                                 (and (= :error (first a)) (= :error (first b))) :both-error
                                 (= :error (first b)) :missing
                                 :else :wrong)}))]
      {:calls (count calls)
       :unbuildable (sort (remove (set (keys calls)) (distinct (map first cands))))
       :results (vec results)})))

(defn report [names]
  (let [{:keys [calls unbuildable results]} (run names)
        by (group-by :status results)]
    (println (format "%d callable overloads; %d names had no synthesisable signature"
                     calls (count unbuildable)))
    (doseq [k [:match :both-error :missing :wrong]]
      (println (format "  %-12s %d" (name k) (count (get by k)))))
    (doseq [k [:wrong :missing]]
      (when (seq (get by k))
        (println (format "\n=== %s ===" (name k)))
        (doseq [{:keys [sql pg ours]} (sort-by :fn (get by k))]
          (println " " sql)
          (println "     PG  " (pr-str pg))
          (println "     ours" (pr-str (update ours 1 #(when % (subs % 0 (min 70 (count %))))))))))
    (when (seq unbuildable)
      (println "\n(no synthesisable signature:" (str/join ", " unbuildable) ")"))))
