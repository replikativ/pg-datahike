(ns datahike.pg.sql.assignment-target
  "Structural assignment targets shared by SQL analysis and write execution.

   Bounds retain their ASTs until execution. An absent slice-bound key means
   omitted; a present SQL NULL expression remains an explicitly supplied bound."
  (:require [datahike.pg.types :as types])
  (:import [net.sf.jsqlparser.schema Column]
           [net.sf.jsqlparser.expression ArrayConstructor RangeExpression JsonExpression Function SignedExpression]))

(def ^:dynamic *omitted-bounds*
  "Internal per-parse annotations keyed by owned target Column identity.
   Cached JSqlParser nodes are never mutated to restore omitted bounds."
  nil)

(defn- json-subexpression [base entries]
  (if (seq entries) (JsonExpression. base (vec entries)) base))

(defn- dimension [expression]
  (let [[slice? lower upper]
        (cond
          (instance? SignedExpression expression)
          (let [signed ^SignedExpression expression
                child (dimension (.getExpression signed))]
            (if (= :slice (:kind child))
              [true (when-let [lower (:lower child)]
                      (SignedExpression. (.getSign signed) lower))
               (:upper child)]
              [false]))

          (instance? RangeExpression expression)
          [true (.getStartExpression ^RangeExpression expression)
           (.getEndExpression ^RangeExpression expression)]

          ;; JSqlParser also represents bracket colons as JSON operators.
          ;; Retain child ASTs (including their parameter ordinals), rather
          ;; than splitting/reparsing their rendered SQL.
          (instance? JsonExpression expression)
          (let [j ^JsonExpression expression
                entries (vec (.getIdentList j))
                colons (keep-indexed #(when (= ":" (.getValue ^java.util.Map$Entry %2)) %1)
                                     entries)]
            (when (next colons)
              (throw (ex-info "invalid array slice" {:sqlstate "42601"})))
            (if-let [i (first colons)]
              [true
               (json-subexpression (.getExpression j) (subvec entries 0 i))
               (json-subexpression (.getKey ^java.util.Map$Entry (nth entries i))
                                   (subvec entries (inc i)))]
              [false]))
          :else [false])]
    (if slice?
      (cond-> {:kind :slice}
        lower (assoc :lower lower)
        upper (assoc :upper upper))
      {:kind :element :index expression})))

(defn describe
  "Decode a target Column without discarding its subscript indirection.
   The caller resolves the base column name/attribute in its own SQL scope."
  [^Column column]
  (let [annotations (when *omitted-bounds*
                      (.get ^java.util.Map *omitted-bounds* column))
        dimensions
        (mapv (fn [i expression]
                (cond-> (dimension expression)
                  (:lower-omitted? (get annotations i)) (dissoc :lower)
                  (:upper-omitted? (get annotations i)) (dissoc :upper)))
              (range)
              (when-let [^ArrayConstructor constructor (.getArrayConstructor column)]
                (.getExpressions constructor)))
        _ (when (> (count dimensions) 6)
            (throw (ex-info "number of array dimensions exceeds the maximum allowed (6)"
                            {:error :program-limit-exceeded :sqlstate "54000"})))
        kind (cond
               (empty? dimensions) :whole
               (some #(= :slice (:kind %)) dimensions) :array-slice
               :else :array-element)]
    {:column (.getColumnName column) :target-kind kind :dimensions dimensions}))

(defn partial-target? [target]
  (boolean (seq (:dimensions target))))

(defn bound-expressions
  "Supplied bounds in PostgreSQL execution order: all upper/index expressions,
   then all lowers. Each entry names the slot to replace with its typed value."
  [{:keys [dimensions]}]
  (into (into []
              (keep-indexed
               (fn [i dimension]
                 (let [key (if (= :element (:kind dimension)) :index :upper)]
                   (when (contains? dimension key)
                     {:path [:dimensions i key] :expression (get dimension key)}))))
              dimensions)
        (keep-indexed (fn [i dimension]
                        (when (contains? dimension :lower)
                          {:path [:dimensions i :lower] :expression (:lower dimension)})))
        dimensions))

(defn value-oid
  "The RHS assignment type. Element targets take the array's element type;
   slices and whole-column targets take the complete column type."
  [target column-oid]
  (if (= :array-element (:target-kind target))
    (get types/array-oid->element-oid column-oid)
    column-oid))

(defn validate-duplicates!
  "Repeated partial targets are legal; a whole target cannot share its base
   column with another target. Call after canonicalizing :column or :attr."
  [targets sqlstate]
  (doseq [[column group] (group-by #(or (:attr %) (:column %)) targets)
          :when (and (next group) (some #(not (partial-target? %)) group))]
    (let [column-name (if (keyword? column) (name column) column)]
      (throw (ex-info (if (= "42701" sqlstate)
                        (str "column \"" column-name "\" specified more than once")
                        (str "multiple assignments to same column \"" column-name "\""))
                      {:error (if (= "42701" sqlstate) :duplicate-column :syntax-error)
                       :sqlstate sqlstate :column column-name}))))
  targets)

(defn program-expression
  "Create a fresh owned AST placeholder and its private program binding.
   No placeholder or program is registered as a user-visible SQL function."
  [program]
  (let [name (str "__pgdh_assignment_" (.replace (str (java.util.UUID/randomUUID)) "-" ""))]
    {:name name
     :expression (doto (Function.) (.setName name))
     :program program}))

(defn reject-partial-default!
  "DEFAULT has no PostgreSQL meaning as a subscript replacement value."
  [target expression]
  (when (and (partial-target? target)
             (instance? Column expression)
             (.equalsIgnoreCase "default" (.getColumnName ^Column expression))
             (nil? (some-> ^Column expression .getTable .getName)))
    (throw (ex-info "cannot set an array element to DEFAULT"
                    {:error :feature-not-supported :sqlstate "0A000"})))
  target)
