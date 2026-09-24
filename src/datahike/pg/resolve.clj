(ns datahike.pg.resolve
  "How a symbol in a query pg-datahike runs becomes a function.

   Datahike resolves the symbols of a query clause through
   `datahike.query.resolve/*symbol-resolver*`. pg-datahike binds it to
   `symbol-resolver` around every call into a query handler
   (`datahike.pg.server/make-query-handler`), and never changes the process's
   own resolver: an application embedding pg-datahike keeps resolving its own
   queries as it did.

   `symbol-resolver` knows the functions the translator emits
   (`datahike.pg.query-fns`, the two secondary-index clauses, the two
   text-search clauses), then Datahike's curated `safe-symbol-resolver`, which
   includes whatever the application registered with
   `datahike.query.resolve/register-fn!`. pg-datahike registers nothing: the
   registry is also what Datahike's own server lets its clients call.

   A binding reaches everything a statement does. A connection's calls run on
   its thread, results are realized before a call returns, Datahike's query
   engine evaluates on the calling thread, and the local writer carries the
   caller's bindings to the writer thread, where CHECK and foreign-key
   enforcement run."
  (:require [datahike.pg.query-fns]
            [datahike.pg.secondary :as secondary]
            [datahike.pg.tsearch :as tsearch]
            [datahike.query.resolve :as dqr]))

(def ^:private by-symbol
  "Emitted symbol -> function. The secondary clauses keep their own names: the
   planner reads their var metadata (`:datahike/external-engine`)."
  (into {'datahike.pg.secondary/candidates          secondary/candidates
         'datahike.pg.secondary/filtered-candidates secondary/filtered-candidates
         'datahike.pg.tsearch/ts-match?             tsearch/ts-match?
         'datahike.pg.tsearch/ts-match3             tsearch/ts-match3}
        (for [[n v] (ns-publics 'datahike.pg.query-fns)]
          [(symbol "datahike.pg.query-fns" (name n)) @v])))

(defn emitted-fn
  "The function the translator names with `sym`, or nil."
  [sym]
  (get by-symbol sym))

(defn- resolve-symbol [sym]
  (or (get by-symbol sym)
      (dqr/safe-symbol-resolver sym)))

(def symbol-resolver
  "Resolve a query symbol the way pg-datahike runs its queries: the functions
   it emits, then Datahike's `safe-symbol-resolver`. One value for the life of
   the process, since Datahike keys its plan and result caches by the
   resolver."
  resolve-symbol)

(defmacro with-symbol-resolver
  "Run `body` with Datahike resolving query symbols through `symbol-resolver`."
  [& body]
  `(binding [dqr/*symbol-resolver* symbol-resolver] ~@body))
