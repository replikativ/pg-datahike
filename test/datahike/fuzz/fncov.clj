(ns datahike.fuzz.fncov
  "Function-coverage sweep against the PostgreSQL oracle.

   Comparing our function TABLE against a list of names understates
   coverage (aggregates and special forms dispatch elsewhere), so this
   CALLS every buildable pg_catalog overload on both servers and diffs the
   answer. Signatures come from the oracle's own pg_proc, so each argument
   list is one PostgreSQL actually accepts. It found `length(bytea)`
   counting the hex text.

   A measurement, not a gate: most of pg_catalog is out of scope, and the
   point is the :wrong list -- a function that answers differently -- and
   the :missing list to prioritise.

     bb fncov                 ; every application-facing pg_catalog function
     bb fncov length lower    ; just these names"
  (:require [clojure.string :as str]
            [datahike.fuzz.differential :as fz])
  (:import [java.sql Connection]))

(set! *warn-on-reflection* true)

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
  (with-open [o (fz/reference-conn)
              t (fz/target-conn)]
    ;; One call must never stall the sweep: the synthesised INTERVAL '1 day'
    ;; made pg_sleep_for sleep a day on the oracle.
    (doseq [c [o t]]
      (fz/exec! c "SET statement_timeout = '10s'")
      ;; pgjdbc sends the client's zone at startup; pin it like the fuzzer.
      (fz/exec! c "SET TimeZone = 'UTC'"))
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
       :unbuildable (sort (remove (set (vals calls)) (distinct (map first cands))))
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

(def ^:private excluded-families
  "pg_catalog families that are server administration, storage, planner or
   index-AM internals -- out of scope by design, and noise in the report."
  (str "^(_|pg_sleep|pg_stat|pg_ls|pg_read|pg_file|pg_log|pg_replication|pg_wal|pg_create|"
       "pg_drop|pg_promote|pg_backup|pg_terminate|pg_cancel|pg_reload|pg_rotate|"
       "binary_upgrade|gin_|gist_|brin|spg|bt|hash|ts_|tsm_|pg_snapshot|txid|"
       "pg_xact|pg_get_wal|pg_import|pg_sequence_last|pg_partition|pg_mcv|heap_|"
       "table_am|index_am|fdw)"))

(defn application-functions
  "Distinct names of the oracle's pg_catalog functions minus type I/O and
   the excluded families."
  []
  (with-open [o (fz/reference-conn)
              st (.createStatement o)
              rs (.executeQuery
                  st (str "SELECT DISTINCT proname FROM pg_proc "
                          "WHERE pronamespace = 'pg_catalog'::regnamespace AND prokind = 'f' "
                          "AND proname !~ '(in|out|recv|send|support|typmodin|typmodout|_handler)$' "
                          "AND proname !~ '" excluded-families "' ORDER BY 1"))]
    (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs 1))) acc))))

(defn -main [& names]
  (report (if (seq names) names (application-functions)))
  (shutdown-agents))
