(ns datahike.pg.arrays.assign
  "Internal complete-value mutation for ordinary PostgreSQL arrays.
   Expressions, assignment casts and row constraints belong to the caller."
  (:require [datahike.pg.arrays :as arrays]))

;; Count reference slots and vector overhead before constructing a result.
;; This is an internal allocation guard, not a PostgreSQL dimension limit.
(def ^:private max-structure-slots (* 1024 1024))
(def ^:private int4-min -2147483648)
(def ^:private int4-max 2147483647)

(defn- fail! [code message]
  (throw (ex-info message {:sqlstate code :error :array-assignment})))

(defn- sql-null? [value]
  (or (nil? value) (= :__null__ value)))

(defn check-subscripts!
  "Check already-coerced subscript values after all bounds have been evaluated.
   Missing slice keys mean omitted bounds; present NULL values are errors.
   More than six dimensions must also be rejected by statement analysis."
  [path]
  (when (> (count path) 6)
    (fail! "54000" "number of array dimensions exceeds the maximum allowed (6)"))
  (let [values (mapcat (fn [dimension]
                         (if (= :element (:kind dimension))
                           [(:index dimension)]
                           (map dimension (filter #(contains? dimension %) [:upper :lower]))))
                       path)]
    (doseq [value values :when (not (sql-null? value))]
      (when-not (integer? value)
        (fail! "42804" "array subscript must have type integer"))
      (when-not (<= int4-min value int4-max)
        (fail! "22003" "integer out of range")))
    (when (some sql-null? values)
      (fail! "22004" "array subscript in assignment must not be null")))
  nil)

(defn- elements [value]
  (if (arrays/array? value) (:elements value) value))

(defn- shape [array]
  (let [dimensions (vec (:dims array))]
    (if (or (nil? array) (empty? (:elements array))
            (some zero? dimensions))
      {:dims [] :lower []}
      {:dims dimensions
       :lower (mapv #(or (get (:lbounds array) %) 1)
                    (range (count dimensions)))})))

(defn- checked-shape! [dims lower]
  (when (> (count dims) 6)
    (fail! "54000" "number of array dimensions exceeds the maximum allowed (6)"))
  (when (some neg? dims)
    (fail! "54000" "array size exceeds the maximum allowed"))
  (doseq [[size bound] (map vector dims lower)]
    ;; PostgreSQL bounds must leave room for their exclusive upper endpoint.
    (when (or (> size int4-max) (< bound int4-min)
              (> (+' bound size) int4-max))
      (fail! "54000" "array lower bound is too large")))
  (loop [sizes dims, prefix 1, slots 0]
    (if-let [size (first sizes)]
      (let [next-prefix (*' prefix size)
            next-slots (+' slots (*' 8 prefix) next-prefix)]
        (when (> next-slots max-structure-slots)
          (fail! "54000" "array assignment exceeds the internal allocation limit"))
        (recur (rest sizes) next-prefix next-slots))
      prefix)))

(defn- empty-array [elem-type]
  (arrays/array elem-type [] [] []))

(defn- flat-elements [array]
  (letfn [(walk [values rank]
            (if (<= rank 1)
              (seq (elements values))
              (mapcat #(walk % (dec rank)) (elements values))))]
    (if (empty? (:dims (shape array)))
      ()
      (walk (:elements array) (count (:dims array))))))

(defn- next-source! [source]
  (if (.hasNext ^java.util.Iterator source)
    (let [value (.next ^java.util.Iterator source)]
      (if (sql-null? value) nil value))
    (fail! "2202E" "source array too small")))

(defn- construct-elements [dims leaf-value]
  (letfn [(build [depth]
            (let [size (nth dims depth)]
              (if (= (inc depth) (count dims))
                (mapv (fn [_] (leaf-value)) (range size))
                (mapv (fn [_] (build (inc depth))) (range size)))))]
    (if (empty? dims) [] (build 0))))

(defn- extend-one-dimensional [values before after]
  (let [prefixed (if (pos? before)
                   (into (vec (repeat before nil)) values)
                   (vec values))]
    (if (pos? after) (into prefixed (repeat after nil)) prefixed)))

(defn- set-element [array elem-type path value]
  (let [{:keys [dims lower]} (shape array)
        indices (mapv :index path)
        rank (count dims)
        value (if (sql-null? value) nil value)]
    (cond
      (empty? dims)
      (let [new-dims (vec (repeat (count indices) 1))]
        (checked-shape! new-dims indices)
        (arrays/array elem-type
                      (construct-elements new-dims (constantly value))
                      new-dims indices))

      (not= rank (count indices))
      (fail! "2202E" "wrong number of array subscripts")

      (= rank 1)
      (let [old-lower (first lower)
            old-upper (dec (+' old-lower (first dims)))
            index (first indices)
            new-lower (min old-lower index)
            new-upper (max old-upper index)
            size (inc (-' new-upper new-lower))
            _ (checked-shape! [size] [new-lower])
            before (- old-lower new-lower)
            after (- new-upper old-upper)
            extended (extend-one-dimensional (:elements array) before after)]
        (arrays/array elem-type (assoc extended (- index new-lower) value)
                      [size] [new-lower]))

      :else
      (let [offsets (mapv - indices lower)]
        (when-not (every? true? (map #(<= 0 %1 (dec %2)) offsets dims))
          (fail! "2202E" "array subscript out of range"))
        (checked-shape! dims lower)
        (letfn [(replace-at [node depth]
                  (let [values (vec (elements node))
                        offset (nth offsets depth)]
                    (assoc values offset
                           (if (= (inc depth) rank)
                             value
                             (replace-at (nth values offset) (inc depth))))))]
          (arrays/array elem-type (replace-at (:elements array) 0) dims lower))))))

(defn- slice-bounds [path dims lower]
  (mapv (fn [i]
          (let [dimension (get path i)
                old-lower (get lower i)
                old-upper (when old-lower (dec (+' old-lower (nth dims i))))]
            (if (= :element (:kind dimension))
              [1 (:index dimension)]
              [(if (contains? dimension :lower) (:lower dimension) old-lower)
               (if (contains? dimension :upper) (:upper dimension) old-upper)])))
        (range (max (count path) (count dims)))))

(defn- replace-slice [node depth offsets spans source]
  (let [values (vec (elements node))
        start (nth offsets depth)
        end (+ start (nth spans depth))]
    (if (= (inc depth) (count spans))
      (persistent!
       (reduce (fn [out i] (assoc! out i (next-source! source)))
               (transient values) (range start end)))
      (reduce (fn [out i]
                (assoc out i (replace-slice (nth out i) (inc depth)
                                            offsets spans source)))
              values (range start end)))))

(defn- set-slice [array elem-type path replacement]
  (let [array (or array (empty-array elem-type))
        {:keys [dims lower]} (shape array)]
    (if (sql-null? replacement)
      array
      (do
        (when-not (arrays/array? replacement)
          (fail! "42804" "array slice assignment requires an array value"))
        (when (and (seq dims) (> (count path) (count dims)))
          (fail! "2202E" "array subscript out of range"))
        (let [bounds (slice-bounds path dims lower)
              lows (mapv first bounds)
              highs (mapv second bounds)]
          (when (some nil? (concat lows highs))
            (fail! "2202E" "array slice subscript must provide both boundaries"))
          (when (and (seq dims) (some true? (map > lows highs)))
            (fail! "2202E" "upper bound cannot be less than lower bound"))
          (let [spans (mapv #(inc (-' %2 %1)) lows highs)
                one-dimensional? (= 1 (count dims))
                old-lower (first lower)
                old-upper (when old-lower (dec (+' old-lower (first dims))))
                result-lower (cond
                               (empty? dims) lows
                               one-dimensional? [(min old-lower (first lows))]
                               :else lower)
                result-dims (cond
                              (empty? dims) spans
                              one-dimensional?
                              [(inc (-' (max old-upper (first highs)) (first result-lower)))]
                              :else dims)]
            (when (and (> (count dims) 1)
                       (not (every? true?
                                    (map (fn [lo hi lb size]
                                           (and (<= lb lo) (< hi (+' lb size))))
                                         lows highs lower dims))))
              (fail! "2202E" "array subscript out of range"))
            (checked-shape! result-dims result-lower)
            (let [needed (checked-shape! spans lows)
                  source-dims (:dims (shape replacement))
                  available (if (empty? source-dims) 0 (reduce *' 1 source-dims))]
              (when (< available needed)
                (fail! "2202E" "source array too small"))
              (let [source (clojure.lang.RT/iter (flat-elements replacement))]
                (cond
                  (zero? needed) (empty-array elem-type)

                  (empty? dims)
                  (arrays/array elem-type
                                (construct-elements spans #(next-source! source))
                                spans lows)

                  one-dimensional?
                  (let [new-lower (first result-lower)
                        new-upper (dec (+' new-lower (first result-dims)))
                        extended (extend-one-dimensional (:elements array)
                                                         (- old-lower new-lower)
                                                         (- new-upper old-upper))]
                    (arrays/array elem-type
                                  (replace-slice extended 0 [(- (first lows) new-lower)] spans source)
                                  result-dims result-lower))

                  :else
                  (arrays/array elem-type
                                (replace-slice (:elements array) 0
                                               (mapv - lows lower) spans source)
                                dims lower))))))))))

(defn assign
  "Return a complete ordinary array value after one evaluated partial assignment.
   The caller checks/coerces indices before evaluating RHS, then calls this with
   the existing container and assignment-coerced RHS. Source slice elements are
   consumed in row-major order, ignoring source bounds and surplus elements."
  [base elem-type path rhs]
  (check-subscripts! path)
  (when (empty? path)
    (fail! "2202E" "wrong number of array subscripts"))
  (let [base (when-not (sql-null? base) base)]
    (when (and base (not (arrays/array? base)))
      (fail! "42804" "array assignment requires an array value"))
    (if (some #(= :slice (:kind %)) path)
      (set-slice base elem-type path rhs)
      (set-element base elem-type path rhs))))
