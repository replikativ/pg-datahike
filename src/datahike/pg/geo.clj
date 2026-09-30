(ns datahike.pg.geo
  "Input and output for PostgreSQL's geometric types (geo_ops.c).

   These were accepted as whatever text was written: `'garbage'::point`
   answered `garbage`, a value that is not a point, in a point column,
   reported as success. That is the passthrough class this campaign
   exists to remove, and it applied to all seven of them.

   Values are held as their CANONICAL text, which is what PostgreSQL
   prints, so storage, comparison and rendering all agree. Canonical
   matters beyond tidiness: `box` sorts its corners so the upper-right
   comes first, so `'(1,2),(3,4)'` and `'(3,4),(1,2)'` are the same box
   and must be the same text.

   The operators (`&&`, `<->`, `~=`, `@>`) are a separate matter; this
   is input, output and identity only."
  (:require [clojure.string :as str]
            [datahike.pg.errors :as errors]
            [datahike.pg.types :as types]))

(defn- bad! [type-name ^String input]
  (throw (errors/pg-error :invalid-text-representation
                          {:type type-name :value input})))

(defn- num-text
  "A coordinate as PostgreSQL prints it.

   This is `float8out` and nothing else, because that is what
   `pair_encode` calls (geo_ops.c). It used to be a second, private
   rendering -- and being second is what made it wrong: an integral
   double went through `(long d)`, so `1e+300` printed as
   9223372036854775807, and it had no scientific notation at all."
  [^double d]
  (types/float->pg-text d false))

(def ^:private special-coords
  "The non-finite spellings `float8in` accepts, lower-cased. Java's
   `parseDouble` knows `Infinity` and `NaN` but not `Inf`, which is how
   PostgreSQL's own regression fixtures write it -- `point_tbl` has
   `(1e+300,Inf)`."
  {"inf" Double/POSITIVE_INFINITY
   "+inf" Double/POSITIVE_INFINITY
   "-inf" Double/NEGATIVE_INFINITY
   "infinity" Double/POSITIVE_INFINITY
   "+infinity" Double/POSITIVE_INFINITY
   "-infinity" Double/NEGATIVE_INFINITY
   "nan" Double/NaN
   "+nan" Double/NaN
   "-nan" Double/NaN})

(def ^:private coord-re
  "What `float8in` accepts as a finite decimal number. Narrower than
   `Double/parseDouble`, which also takes a `d` or `f` suffix --
   `'(1d,2)'::point` is an error in PostgreSQL."
  #"[+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?")

(def ^:private hex-coord-re
  "`float8in` is `strtod`, and strtod reads hex: `'(0x10,2)'::point` is
   `(16,2)`. Java reads a hex FLOAT (`0x1p4`) but not a bare hex
   integer, so one gets the exponent strtod would have defaulted to."
  #"[+-]?0[xX][0-9a-fA-F]*\.?[0-9a-fA-F]*(?:[pP][+-]?\d+)?")

(defn- parse-coord
  "One coordinate, or nil when the text is not a float PostgreSQL would
   accept."
  [^String t]
  (if-let [special (get special-coords (str/lower-case t))]
    special
    (let [t (cond
              (re-matches coord-re t) t
              (re-matches hex-coord-re t)
              (if (re-find #"[pP]" t) t (str t "p0"))
              :else nil)]
      (when t
        (try (Double/parseDouble t) (catch Exception _ nil))))))

(defn- numbers
  "Every number in `s`, in order, or nil when `s` holds anything that is
   not a number, a delimiter or whitespace. Delimiters are structural
   and are checked by the caller against the shape it expects."
  [^String s]
  (let [cleaned (str/replace s #"[\(\)\[\]<>{},]" " ")
        toks (remove str/blank? (str/split (str/trim cleaned) #"\s+"))]
    (when (seq toks)
      (reduce (fn [acc t]
                (if-let [d (parse-coord t)]
                  (conj acc (double d))
                  (reduced nil)))
              []
              toks))))

(defn- pt [x y] (str "(" (num-text x) "," (num-text y) ")"))

(defn- delimiters
  "The structural characters of `s`, in order, so a shape can be checked
   without a parser: `((1,2),(3,4))` is `(()())`."
  [^String s]
  (str/join (re-seq #"[\(\)\[\]<>{}]" s)))

(defn point-in [^String s type-name]
  (let [ns' (numbers s)
        d (delimiters s)]
    (when-not (and ns' (= 2 (count ns')) (contains? #{"" "()"} d))
      (bad! type-name s))
    (pt (nth ns' 0) (nth ns' 1))))

(defn lseg-in [^String s type-name]
  (let [ns' (numbers s)
        d (delimiters s)]
    (when-not (and ns' (= 4 (count ns'))
                   (contains? #{"" "()()" "[()()]" "(()())"} d))
      (bad! type-name s))
    (str "[" (pt (nth ns' 0) (nth ns' 1)) "," (pt (nth ns' 2) (nth ns' 3)) "]")))

(defn box-in [^String s type-name]
  (let [ns' (numbers s)
        d (delimiters s)]
    (when-not (and ns' (= 4 (count ns'))
                   (contains? #{"" "()()" "(()())"} d))
      (bad! type-name s))
    (let [[x1 y1 x2 y2] ns'
          ;; box_in sorts the corners: the high one is printed first, so
          ;; two spellings of the same box are one value.
          hx (max x1 x2) hy (max y1 y2)
          lx (min x1 x2) ly (min y1 y2)]
      (str (pt hx hy) "," (pt lx ly)))))

(defn- point-list [^String s type-name expect-open?]
  (let [ns' (numbers s)]
    (when-not (and ns' (even? (count ns')) (pos? (count ns')))
      (bad! type-name s))
    (let [pts (map (fn [[x y]] (pt x y)) (partition 2 ns'))]
      (if expect-open?
        (str "[" (str/join "," pts) "]")
        (str "(" (str/join "," pts) ")")))))

(defn path-in [^String s type-name]
  (let [t (str/trim s)
        open? (str/starts-with? t "[")]
    (when-not (re-matches #"[\s\(\)\[\],0-9eE.+-]+" t) (bad! type-name s))
    (point-list t type-name open?)))

(defn polygon-in [^String s type-name]
  (when-not (re-matches #"[\s\(\),0-9eE.+-]+" (str/trim s)) (bad! type-name s))
  (point-list s type-name false))

(defn circle-in [^String s type-name]
  (let [ns' (numbers s)
        d (delimiters s)]
    (when-not (and ns' (= 3 (count ns'))
                   (contains? #{"" "()" "<()>" "(())"} d))
      (bad! type-name s))
    (str "<" (pt (nth ns' 0) (nth ns' 1)) "," (num-text (nth ns' 2)) ">")))

(defn line-in [^String s type-name]
  (let [ns' (numbers s)
        d (delimiters s)]
    (cond
      ;; `{A,B,C}` -- the equation itself.
      (and ns' (= 3 (count ns')) (= "{}" d))
      (let [[a b c] ns']
        (when (and (zero? a) (zero? b)) (bad! type-name s))
        (str "{" (num-text a) "," (num-text b) "," (num-text c) "}"))

      ;; Two points. line_in computes Ax + By + C = 0 through them and
      ;; divides through so the leading non-zero coefficient is 1.
      (and ns' (= 4 (count ns')) (contains? #{"" "()()" "[()()]" "(()())"} d))
      (let [[x1 y1 x2 y2] ns']
        (when (and (== x1 x2) (== y1 y2)) (bad! type-name s))
        (let [a (- y2 y1)
              b (- x1 x2)
              c (- (* x2 y1) (* x1 y2))
              k (if (zero? a) b a)]
          (str "{" (num-text (/ a k)) "," (num-text (/ b k)) ","
               (num-text (/ c k)) "}")))

      :else (bad! type-name s))))

(def ^:private by-type
  {"point" point-in "lseg" lseg-in "box" box-in "path" path-in
   "polygon" polygon-in "circle" circle-in "line" line-in})

(defn geometric-type? [type-name] (contains? by-type type-name))

(defn geometric-in
  "Canonical text for `v` as `type-name`, or 22P02 when it is not one."
  [type-name v]
  (if-let [f (get by-type type-name)]
    (f (str v) type-name)
    (str v)))
