(ns datahike.pg.vector
  "The pgvector `vector` scalar represented by Datahike's native float array.

   The public functions in this namespace follow pgvector 0.8.6's vector.c:
   elements and most distance accumulators use IEEE float32, while norm uses
   a float64 accumulator.  Proximum is deliberately not involved here; an ANN
   index may supply candidates later, but SQL scalar results remain exact."
  (:require [clojure.string :as str]
            [datahike.pg.types :as types]))

(set! *warn-on-reflection* true)

(def ^:const max-dimensions 16000)

(def ^:private float-array-class (Class/forName "[F"))

(defn vector-value? [v]
  (instance? float-array-class v))

(defn- pg-error! [sqlstate message & [data]]
  (throw (ex-info message (merge {:sqlstate sqlstate} data))))

(defn check-dimensions!
  ([dim]
   (cond
     (< dim 1)
     (pg-error! "22000" "vector must have at least 1 dimension")

     (> dim max-dimensions)
     (pg-error! "54000"
                (str "vector cannot have more than " max-dimensions " dimensions"))))
  ([expected actual]
   (check-dimensions! actual)
   (when (and (some? expected) (not= -1 expected) (not= expected actual))
     (pg-error! "22000" (str "expected " expected " dimensions, not " actual)))))

(defn check-same-dimensions! [^floats a ^floats b]
  (let [ad (alength a)
        bd (alength b)]
    (when (not= ad bd)
      (pg-error! "22000" (str "different vector dimensions " ad " and " bd)))))

(defn validate!
  "Validate a native float array at every boundary. Datahike accepts arbitrary
   float arrays, including empty and non-finite values, while pgvector does
   not; callers must not infer validity merely from the runtime class."
  [^floats v]
  (check-dimensions! (alength v))
  (dotimes [i (alength v)]
    (let [x (aget v i)]
      (when-not (Float/isFinite x)
        (pg-error! "22000" (if (Float/isNaN x)
                             "NaN not allowed in vector"
                             "infinite value not allowed in vector")))))
  v)

(defn parse-typmod
  "Return the dimension declared by vector(n), nil for bare vector. Rejects
   the same modifier shapes as pgvector's vector_typmod_in."
  [type-name]
  (let [s (str/lower-case (str/trim (str type-name)))
        args (some-> (re-find #"^vector\s*\((.*)\)$" s) second)
        parts (when args (mapv str/trim (str/split args #"," -1)))]
    (when parts
      (when (not= 1 (count parts))
        (pg-error! "22023" "invalid type modifier"))
      (let [raw (first parts)
            n (try (Long/parseLong raw)
                   (catch Exception _
                     (pg-error! "22P02"
                                (str "invalid input syntax for type integer: " (pr-str raw)))))]
        (cond
          (< n 1) (pg-error! "22023" "dimensions for type vector must be at least 1")
          (> n max-dimensions)
          (pg-error! "22023"
                     (str "dimensions for type vector cannot exceed " max-dimensions))
          :else n)))))

(def ^:private float-token
  #"[+-]?(?:(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?|(?i:NaN|Inf(?:inity)?))")

(defn parse
  "Parse pgvector's text input into a primitive float array. `typmod` is the
   declared dimension, or nil/-1 for unconstrained `vector`."
  ([value] (parse value nil))
  ([value typmod]
   (if (vector-value? value)
     (do (validate! value)
         (check-dimensions! typmod (alength ^floats value))
         value)
     (let [input (str value)
           s (str/trim input)]
       (when-not (and (str/starts-with? s "[") (str/ends-with? s "]"))
         (pg-error! "22P02" (str "invalid input syntax for type vector: " (pr-str input))))
       (let [body (subs s 1 (dec (count s)))
             tokens (if (str/blank? body) [] (str/split body #"," -1))]
         (check-dimensions! typmod (count tokens))
         (let [result (float-array (count tokens))]
           (doseq [[i raw] (map-indexed vector tokens)]
             (let [token (str/trim raw)]
               (when-not (re-matches float-token token)
                 (pg-error! "22P02"
                            (str "invalid input syntax for type vector: " (pr-str input))))
               (let [v (try
                         (Float/parseFloat token)
                         (catch NumberFormatException _
                           (pg-error! "22P02"
                                      (str "invalid input syntax for type vector: "
                                           (pr-str input)))))]
                 (cond
                   (Float/isNaN v) (pg-error! "22000" "NaN not allowed in vector")
                   (Float/isInfinite v)
                   (if (re-find #"(?i)inf(?:inity)?$" token)
                     (pg-error! "22000" "infinite value not allowed in vector")
                     (pg-error! "22003"
                                (str (pr-str token) " is out of range for type vector")))
                   :else (aset-float result i v)))))
           result))))))

(defn coerce
  "Return `value` as a vector, parsing PostgreSQL text when necessary."
  ([value] (coerce value nil))
  ([value typmod] (parse value typmod)))

(defn to-pg-text [^floats v]
  (validate! v)
  (str "["
       (str/join "," (map #(types/float->pg-text % true) v))
       "]"))

(defn vector-dims [^floats v] (alength v))

(defn vector-norm [^floats v]
  (Math/sqrt
   (loop [i 0, sum 0.0]
     (if (< i (alength v))
       (let [x (double (aget v i))]
         (recur (inc i) (+ sum (* x x))))
       sum))))

(defn- float32-sum [^floats a ^floats b term]
  (check-same-dimensions! a b)
  (loop [i 0, sum (float 0.0)]
    (if (< i (alength a))
      (recur (inc i) (float (+ sum (float (term (aget a i) (aget b i))))))
      sum)))

(defn l2-squared-distance [^floats a ^floats b]
  (double
   (float32-sum a b (fn [x y]
                      (let [d (float (- (float x) (float y)))]
                        (float (* d d)))))))

(defn l2-distance [^floats a ^floats b]
  (Math/sqrt (l2-squared-distance a b)))

(defn inner-product [^floats a ^floats b]
  (double (float32-sum a b #(float (* (float %1) (float %2))))))

(defn negative-inner-product [^floats a ^floats b]
  (- (inner-product a b)))

(defn cosine-distance [^floats a ^floats b]
  (check-same-dimensions! a b)
  (let [[similarity norma normb]
        (loop [i 0, similarity (float 0.0), norma (float 0.0), normb (float 0.0)]
          (if (< i (alength a))
            (let [x (aget a i)
                  y (aget b i)]
              (recur (inc i)
                     (float (+ similarity (float (* x y))))
                     (float (+ norma (float (* x x))))
                     (float (+ normb (float (* y y))))))
            [similarity norma normb]))
        similarity (/ (double similarity)
                      (Math/sqrt (* (double norma) (double normb))))]
    (cond
      (Double/isNaN similarity) Double/NaN
      (> similarity 1.0) 0.0
      (< similarity -1.0) 2.0
      :else (- 1.0 similarity))))

(defn l1-distance [^floats a ^floats b]
  (double
   (float32-sum a b #(float (Math/abs (float (- (float %1) (float %2))))))))

(defn compare-values
  "pgvector's btree comparison: elements first, then dimensionality. Float
   comparisons intentionally make -0 and +0 equal."
  [a b]
  (let [^floats a (coerce a)
        ^floats b (coerce b)
        n (min (alength a) (alength b))]
    (loop [i 0]
      (if (< i n)
        (let [x (aget a i), y (aget b i)]
          (cond (< x y) -1 (> x y) 1 :else (recur (inc i))))
        (compare (alength a) (alength b))))))
