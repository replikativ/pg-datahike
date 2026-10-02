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
            [datahike.pg.input :as input]
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

(defn- float8-lt
  "`float8_lt` (float.h), which is NOT `<`: NaN is larger than
   everything. `box_in` reorders its corners with it, so
   `'(1,1),(NaN,0)'::box` swaps the x coordinates and keeps the y, which
   `max`/`min` cannot express -- they propagate NaN into both."
  [^double a ^double b]
  (and (not (Double/isNaN a))
       (or (Double/isNaN b) (< a b))))

;; ---------------------------------------------------------------------------
;; The scanner. These were three regexes over the whole string plus a
;; `delimiters` shape check, which could not see WHERE a character sat:
;; the comma between a point's coordinates was never required
;; (`'(1 2)'::point` was accepted), an unbalanced polygon was accepted,
;; and the character class gating path and polygon excluded the letters
;; of `Inf`, `NaN` and `0x`, so those forms worked for point/lseg/box and
;; not for path/polygon. `pair_decode` and `path_decode` are small
;; scanners; this is them.

(defn- skip-ws ^long [^String s ^long i]
  (let [n (.length s)]
    (loop [i i]
      (if (and (< i n) (Character/isWhitespace (.charAt s i))) (recur (inc i)) i))))

(defn- at ^long [^String s ^long i]
  (if (< i (.length s)) (int (.charAt s i)) -1))

(defn- single-decode
  "One coordinate. `[value end-index]`, or nil."
  [^String s ^long i type-name orig]
  (or (input/float8-prefix s i)
      (bad! type-name orig)))

(defn- pair-decode
  "`pair_decode`: an optional `(`, a float, a REQUIRED comma, a float,
   and the matching `)` if the `(` was there. `[x y end-index]`."
  [^String s ^long i type-name orig]
  (let [i (skip-ws s i)
        has-delim? (= (int \() (at s i))
        i (if has-delim? (inc i) i)
        [x i] (single-decode s i type-name orig)
        _ (when-not (= (int \,) (at s i)) (bad! type-name orig))
        [y i] (single-decode s (inc i) type-name orig)]
    (if has-delim?
      (do (when-not (= (int \)) (at s i)) (bad! type-name orig))
          [x y (skip-ws s (inc i))])
      [x y i])))

(defn- path-decode
  "`path_decode`. Returns `{:points [[x y] …] :open? bool :end i}`.
   `opentype?` is whether `[` is allowed; `npts` is how many pairs to
   read. The leading-paren dance is PostgreSQL's: a second `(` means a
   wrapper, and so does a lone `(` that is the string's last one -- which
   is what makes the documented `(1,2,3,4)` form work for box."
  [^String s i0 opentype? npts type-name orig]
  (let [i (skip-ws s (long i0))
        open? (= (int \[) (at s i))
        _ (when (and open? (not opentype?)) (bad! type-name orig))
        [depth i] (cond
                    open? [1 (inc i)]
                    (= (int \() (at s i))
                    (let [cp (skip-ws s (inc i))]
                      (if (or (= (int \() (at s cp))
                              (= i (.lastIndexOf s (int \())))
                        [1 cp]
                        [0 i]))
                    :else [0 i])
        npts (long npts)
        [pts i] (loop [k 0, acc [], i i]
                  (if (= k npts)
                    [acc i]
                    (let [[x y i] (pair-decode s i type-name orig)
                          i (if (= (int \,) (at s i)) (inc i) i)]
                      (recur (inc k) (conj acc [x y]) i))))
        i (loop [depth depth, i i]
            (if (zero? depth)
              i
              (if (or (= (int \)) (at s i))
                      (and (= (int \]) (at s i)) open? (= 1 depth)))
                (recur (dec depth) (skip-ws s (inc i)))
                (bad! type-name orig))))]
    {:points pts :open? open? :end i}))

(defn- end-of-string! [^String s ^long i type-name orig]
  (when-not (= i (.length s)) (bad! type-name orig))
  i)

(defn- pair-count
  "`pair_count`: the number of points is half the commas, rounded up, and
   an EVEN comma count is no shape at all. This is how a path or polygon
   learns its length before it is read, and it is why `'((1,2)'` is one
   point rather than a syntax error at the count stage."
  ^long [^String s]
  (let [n (count (filter #(= \, %) s))]
    (if (odd? n) (quot (inc n) 2) -1)))

(defn- pt [x y] (str "(" (num-text x) "," (num-text y) ")"))

(defn point-in [^String s type-name]
  (let [[x y i] (pair-decode s 0 type-name s)]
    (end-of-string! s i type-name s)
    (pt x y)))

(defn lseg-in [^String s type-name]
  (let [{:keys [points end]} (path-decode s 0 true 2 type-name s)]
    (end-of-string! s end type-name s)
    (let [[[x1 y1] [x2 y2]] points]
      (str "[" (pt x1 y1) "," (pt x2 y2) "]"))))

(defn box-in [^String s type-name]
  (let [{:keys [points end]} (path-decode s 0 false 2 type-name s)]
    (end-of-string! s end type-name s)
    (let [[[hx hy] [lx ly]] points
          ;; box_in reorders each coordinate independently, with
          ;; float8_lt -- not with max/min over the pair.
          [hx lx] (if (float8-lt hx lx) [lx hx] [hx lx])
          [hy ly] (if (float8-lt hy ly) [ly hy] [hy ly])]
      (str (pt hx hy) "," (pt lx ly)))))

(defn path-in [^String s type-name]
  (let [npts (pair-count s)]
    (when (<= npts 0) (bad! type-name s))
    (let [i (skip-ws s 0)
          ;; path_in peels ONE leading paren of its own when it is the
          ;; string's last `(` -- the `(1,2,3,4)` form -- and
          ;; path_decode then does its own.
          [depth i] (if (and (= (int \() (at s i))
                             (= i (.lastIndexOf s (int \())))
                      [1 (inc i)]
                      [0 i])
          {:keys [points open? end]} (path-decode s i true npts type-name s)
          end (if (pos? depth)
                (do (when-not (= (int \)) (at s end)) (bad! type-name s))
                    (skip-ws s (inc end)))
                end)]
      (end-of-string! s end type-name s)
      (let [body (str/join "," (map (fn [[x y]] (pt x y)) points))]
        (if open? (str "[" body "]") (str "(" body ")"))))))

(defn polygon-in [^String s type-name]
  (let [npts (pair-count s)]
    (when (<= npts 0) (bad! type-name s))
    (let [{:keys [points end]} (path-decode s 0 false npts type-name s)]
      (end-of-string! s end type-name s)
      (str "(" (str/join "," (map (fn [[x y]] (pt x y)) points)) ")"))))

(defn circle-in [^String s type-name]
  (let [i (skip-ws s 0)
        [depth i] (cond
                    (= (int \<) (at s i)) [1 (inc i)]
                    (= (int \() (at s i))
                    (let [cp (skip-ws s (inc i))]
                      (if (= (int \() (at s cp)) [1 cp] [0 i]))
                    :else [0 i])
        [cx cy i] (pair-decode s i type-name s)
        i (if (= (int \,) (at s i)) (inc i) i)
        [r i] (single-decode s i type-name s)
        i (skip-ws s i)
        i (if (pos? depth)
            (if (or (= (int \>) (at s i)) (= (int \)) (at s i)))
              (skip-ws s (inc i))
              (bad! type-name s))
            i)]
    (end-of-string! s i type-name s)
    ;; circle_in rejects a negative radius and must ACCEPT NaN, so the
    ;; test is `< 0`, which NaN fails.
    (when (< (double r) 0.0) (bad! type-name s))
    (str "<" (pt cx cy) "," (num-text r) ">")))

(defn- line-construct
  "`line_construct` (geo_ops.c:1083). The canonical form is NOT `divide
   through by the leading non-zero coefficient` -- it is `mx - y + b =
   0`, with two special cases. Dividing through gave a different stored
   value for every slope that is not +/-1: `'(1,1),(3,5)'::line` was
   `{1,-0.5,-0.5}` where PostgreSQL stores `{2,-1,-1}`."
  [^double px ^double py ^double m]
  (cond
    (Double/isInfinite m) [-1.0 0.0 px]
    (zero? m)             [0.0 -1.0 py]
    :else                 (let [c (- py (* m px))]
                            [m -1.0 (if (zero? c) 0.0 c)])))

(defn- point-sl
  "`point_sl`: the slope, with FPeq's exact comparisons, so a vertical
   line is +Infinity and a horizontal one is 0."
  ^double [^double x1 ^double y1 ^double x2 ^double y2]
  (cond
    (== x1 x2) Double/POSITIVE_INFINITY
    (== y1 y2) 0.0
    :else      (/ (- y1 y2) (- x1 x2))))

(defn line-in [^String s type-name]
  (let [t (skip-ws s 0)]
    (if (= (int \{) (at s t))
      ;; `{A,B,C}` -- the equation itself.
      (let [[a i] (single-decode s (inc t) type-name s)
            _ (when-not (= (int \,) (at s i)) (bad! type-name s))
            [b i] (single-decode s (inc i) type-name s)
            _ (when-not (= (int \,) (at s i)) (bad! type-name s))
            [c i] (single-decode s (inc i) type-name s)
            i (skip-ws s i)]
        (when-not (= (int \}) (at s i)) (bad! type-name s))
        (end-of-string! s (skip-ws s (inc i)) type-name s)
        (when (and (zero? (double a)) (zero? (double b)))
          (throw (errors/pg-error
                  :invalid-text-representation
                  {:message (str "invalid line specification: A and B cannot"
                                 " both be zero")})))
        (str "{" (num-text a) "," (num-text b) "," (num-text c) "}"))
      ;; Two points.
      (let [{:keys [points end]} (path-decode s 0 true 2 type-name s)]
        (end-of-string! s end type-name s)
        (let [[[x1 y1] [x2 y2]] points]
          (when (and (== (double x1) (double x2)) (== (double y1) (double y2)))
            (throw (errors/pg-error
                    :invalid-text-representation
                    {:message (str "invalid line specification: must be two"
                                   " distinct points")})))
          (let [[a b c] (line-construct x1 y1 (point-sl x1 y1 x2 y2))]
            (str "{" (num-text a) "," (num-text b) "," (num-text c) "}")))))))

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

;; ---------------------------------------------------------------------------
;; Comparison
;;
;; There is no btree opclass for ANY geometric type in PostgreSQL --
;; `select count(*) from pg_opclass where opcintype::regtype::text in
;; (…) and opcmethod=403` is 0 -- so "canonical text, therefore text
;; order is the type's order" was false for all seven. The operators
;; that do exist compare AREA (box, circle), LENGTH (lseg) or POINT
;; COUNT (path), none of which is lexicographic in any reading, and
;; several spellings have no operator at all: `point = point` and
;; `box <> box` are 42883 in PostgreSQL and were `t`/`f` here.

(def ^:private epsilon 1.0E-06)

(defn- fpeq [^double a ^double b] (or (== a b) (<= (Math/abs (- a b)) epsilon)))
(defn- fplt [^double a ^double b] (< (+ a epsilon) b))
(defn- fple [^double a ^double b] (<= a (+ b epsilon)))
(defn- fpgt [^double a ^double b] (> a (+ b epsilon)))
(defn- fpge [^double a ^double b] (>= (+ a epsilon) b))

(defn- coords
  "Every number in a value we ourselves canonicalised, in order. Safe
   because the text came out of the input functions above."
  [^String s]
  (loop [i 0, acc []]
    (if-let [[v e] (input/float8-prefix s i)]
      (recur (long e) (conj acc (double v)))
      (if (>= i (.length s))
        acc
        (recur (inc i) acc)))))

(defn- box-area ^double [s]
  (let [[hx hy lx ly] (coords s)] (* (- hx lx) (- hy ly))))

(defn- circle-area ^double [s]
  (let [[_ _ r] (coords s)] (* r r Math/PI)))

(defn- lseg-length ^double [s]
  (let [[x1 y1 x2 y2] (coords s)] (Math/hypot (- x1 x2) (- y1 y2))))

(defn- path-npts ^long [s] (quot (count (coords s)) 2))

(defn- points-eq? [[x1 y1] [x2 y2]] (and (fpeq x1 x2) (fpeq y1 y2)))

(defn- lseg-eq? [a b]
  (let [[ax1 ay1 ax2 ay2] (coords a)
        [bx1 by1 bx2 by2] (coords b)]
    (and (points-eq? [ax1 ay1] [bx1 by1])
         (points-eq? [ax2 ay2] [bx2 by2]))))

(defn- line-eq?
  "`line_eq`: equal after scaling by the ratio of the A (or B)
   coefficients, so `{1,2,3}` and `{2,4,6}` are one line."
  [a b]
  (let [[a1 b1 c1] (coords a)
        [a2 b2 c2] (coords b)
        ratio (cond (not (zero? a2)) (/ a1 a2)
                    (not (zero? b2)) (/ b1 b2)
                    :else nil)]
    (if ratio
      (and (fpeq a1 (* ratio a2)) (fpeq b1 (* ratio b2)) (fpeq c1 (* ratio c2)))
      (and (fpeq a1 a2) (fpeq b1 b2) (fpeq c1 c2)))))

(def comparison-operators
  "Which of `= <> < > <= >=` PostgreSQL actually has for each type, read
   off pg_operator. `~=` (`same as`) is left out: it does not parse here
   yet. Note the gaps -- box and path have no `<>`, point has no `=`,
   and polygon has none of the six."
  {"point"   #{'not=}
   "lseg"    #{'= 'not= '< '> '<= '>=}
   "path"    #{'= '< '> '<= '>=}
   "box"     #{'= '< '> '<= '>=}
   "polygon" #{}
   "line"    #{'=}
   "circle"  #{'= 'not= '< '> '<= '>=}})

(defn comparison-exists?
  "Does PostgreSQL have `op` for two operands of `type-name`?"
  [type-name op]
  (contains? (get comparison-operators type-name #{}) op))

(defn compare-values
  "`a op b` for two canonical values of `type-name`. Only called for the
   combinations `comparison-exists?` admits."
  [type-name op a b]
  (case type-name
    ("box" "circle")
    (let [area (if (= "box" type-name) box-area circle-area)
          x (double (area a)), y (double (area b))]
      (case op
        = (fpeq x y), not= (not (fpeq x y))
        < (fplt x y), > (fpgt x y), <= (fple x y), >= (fpge x y)))

    "path"
    (let [x (path-npts a), y (path-npts b)]
      (case op
        = (= x y), not= (not= x y)
        < (< x y), > (> x y), <= (<= x y), >= (>= x y)))

    "lseg"
    (case op
      ;; lseg_eq/lseg_ne are the ENDPOINTS; the orderings are the LENGTH.
      = (lseg-eq? a b)
      not= (not (lseg-eq? a b))
      (let [x (lseg-length a), y (lseg-length b)]
        (case op
          < (fplt x y), > (fpgt x y), <= (fple x y), >= (fpge x y))))

    "line" (case op = (line-eq? a b))

    "point" (case op not= (not (points-eq? (vec (take 2 (coords a)))
                                           (vec (take 2 (coords b))))))))

(defn geometric-length
  "`lseg_length` / `path_length`: the sum of the segment lengths, which
   `length()` answered with the CHARACTER COUNT of the canonical text --
   13 for `[(0,0),(3,4)]`, where PostgreSQL says 5. A plausible number
   with no error, which is the worst shape a wrong answer takes."
  ^double [type-name ^String s]
  (let [cs (coords s)
        pts (partition 2 cs)]
    (case type-name
      "lseg" (lseg-length s)
      "path" (let [closed? (str/starts-with? (str/trim s) "(")
                   segs (if closed? (concat pts [(first pts)]) pts)]
               (reduce + 0.0 (map (fn [[[x1 y1] [x2 y2]]]
                                    (Math/hypot (- x1 x2) (- y1 y2)))
                                  (partition 2 1 segs))))
      0.0)))

;; ---------------------------------------------------------------------------
;; Containment and overlap: `&&`, `@>`, `<@`
;;
;; These reached `expr.clj`'s array/jsonb fall-through, where two
;; geometric strings are neither an array nor jsonb, so the answer was
;; a bare `false` -- `'(0,0),(1,1)'::box && '(0,0),(2,2)'::box` was `f`
;; where PostgreSQL says `t`. Of PostgreSQL's 21 pairs for these three
;; operators, the 14 implemented here are the ones whose definition is
;; a comparison or a distance; the 7 polygon and path pairs need
;; `point_inside`'s crossing-number walk and `lseg_inside_poly`, and
;; refuse honestly rather than answering.

(defn- pt-of [s] (let [[x y] (coords s)] [x y]))
(defn- box-of [s] (let [[hx hy lx ly] (coords s)] [hx hy lx ly]))
(defn- circ-of [s] (let [[cx cy r] (coords s)] [cx cy r]))
(defn- lseg-of [s] (let [[x1 y1 x2 y2] (coords s)] [x1 y1 x2 y2]))

(defn- point-dt ^double [^double x1 ^double y1 ^double x2 ^double y2]
  (Math/hypot (- x1 x2) (- y1 y2)))

(defn- box-ov? [a b]
  (let [[ahx ahy alx aly] (box-of a) [bhx bhy blx bly] (box-of b)]
    (and (fple alx bhx) (fple blx ahx) (fple aly bhy) (fple bly ahy))))

(defn- box-contain-box? [a b]
  (let [[ahx ahy alx aly] (box-of a) [bhx bhy blx bly] (box-of b)]
    (and (fpge ahx bhx) (fple alx blx) (fpge ahy bhy) (fple aly bly))))

(defn- box-contain-point? [b p]
  (let [[hx hy lx ly] (box-of b) [x y] (pt-of p)]
    ;; `box_contain_point` uses EXACT comparisons, not FP*.
    (and (>= hx x) (<= lx x) (>= hy y) (<= ly y))))

(defn- circle-overlap? [a b]
  (let [[ax ay ar] (circ-of a) [bx by br] (circ-of b)]
    (fple (point-dt ax ay bx by) (+ ar br))))

(defn- circle-contain-circle? [a b]
  (let [[ax ay ar] (circ-of a) [bx by br] (circ-of b)]
    (fple (point-dt ax ay bx by) (- ar br))))

(defn- circle-contain-point? [c p]
  (let [[cx cy r] (circ-of c) [x y] (pt-of p)]
    (<= (point-dt cx cy x y) r)))

(defn- line-contain-point? [l p]
  (let [[a b c] (coords l) [x y] (pt-of p)]
    (<= (Math/abs (+ (* a x) (* b y) c)) epsilon)))

(defn- lseg-contain-point? [l p]
  (let [[x1 y1 x2 y2] (lseg-of l) [x y] (pt-of p)]
    (fpeq (+ (point-dt x y x1 y1) (point-dt x y x2 y2))
          (point-dt x1 y1 x2 y2))))

(defn- lseg-in-line? [s l]
  (let [[x1 y1 x2 y2] (lseg-of s)]
    (and (line-contain-point? l (pt x1 y1))
         (line-contain-point? l (pt x2 y2)))))

(defn- lseg-in-box? [s b]
  (let [[x1 y1 x2 y2] (lseg-of s)]
    (and (box-contain-point? b (pt x1 y1))
         (box-contain-point? b (pt x2 y2)))))

(def ^:private point-on-polygon
  "`POINT_ON_POLYGON`, which `lseg_crossing` returns in place of a
   crossing count when the point lies ON the boundary."
  Long/MAX_VALUE)

(defn- lseg-crossing
  "`lseg_crossing` (geo_ops.c:5339). The crossing contribution of one
   edge of a polygon translated so the test point is the origin."
  ^long [^double x ^double y ^double prev-x ^double prev-y]
  (let [fpzero? (fn [^double v] (<= (Math/abs v) epsilon))]
    (if (fpzero? y)
      (cond
        (fpzero? x) point-on-polygon
        (fpgt x 0.0) (if (fpzero? prev-y)
                       (if (fpgt prev-x 0.0) 0 point-on-polygon)
                       (if (fplt prev-y 0.0) 1 -1))
        :else (if (fpzero? prev-y)
                (if (fplt prev-x 0.0) 0 point-on-polygon)
                0))
      (let [y-sign (if (fpgt y 0.0) 1 -1)]
        (cond
          (fpzero? prev-y) (if (fplt prev-x 0.0) 0 y-sign)
          (or (and (neg? y-sign) (fplt prev-y 0.0))
              (and (pos? y-sign) (fpgt prev-y 0.0)))
          0
          :else
          (cond
            (and (fpge x 0.0) (fpgt prev-x 0.0)) (* 2 y-sign)
            (and (fplt x 0.0) (fple prev-x 0.0)) 0
            :else
            (let [z (- (* (- x prev-x) y) (* (- y prev-y) x))]
              (cond
                (fpzero? z) point-on-polygon
                (or (and (neg? y-sign) (fplt z 0.0))
                    (and (pos? y-sign) (fpgt z 0.0))) 0
                :else (* 2 y-sign)))))))))

(defn- point-inside?
  "`point_inside` (geo_ops.c:5401): the crossing-number walk, with
   `POINT_ON_POLYGON` short-circuiting to inside. `pts` is the vertex
   list as [[x y] …]."
  [[^double px ^double py] pts]
  (let [[[fx fy] & _] pts
        x0 (- (double fx) px)
        y0 (- (double fy) py)]
    (loop [[[vx vy] & more] (rest pts)
           prev-x x0, prev-y y0, total 0]
      (if (nil? vx)
        (let [c (lseg-crossing x0 y0 prev-x prev-y)]
          (if (= point-on-polygon c)
            true
            (not (zero? (+ total c)))))
        (let [x (- (double vx) px)
              y (- (double vy) py)
              c (lseg-crossing x y prev-x prev-y)]
          (if (= point-on-polygon c)
            true
            (recur more x y (+ total c))))))))

(defn- vertices [s] (mapv vec (partition 2 (coords s))))

(defn- poly-contain-point? [poly p] (point-inside? (pt-of p) (vertices poly)))

(defn- path-contain-point?
  "`path_contain_pt`, which is `point_inside` over the path's points --
   PostgreSQL applies it to an open path too."
  [path p]
  (point-inside? (pt-of p) (vertices path)))

(def ^:private spatial-ops
  "`[op left-type right-type] -> (fn [left right] boolean)`, keyed the
   way pg_operator is. Only the pairs PostgreSQL has: anything else is
   `operator does not exist`, which is the honest answer for, say,
   `point && point`."
  {["&&" "box" "box"]         box-ov?
   ["&&" "circle" "circle"]   circle-overlap?
   ["@>" "box" "box"]         box-contain-box?
   ["<@" "box" "box"]         (fn [a b] (box-contain-box? b a))
   ["@>" "box" "point"]       box-contain-point?
   ["<@" "point" "box"]       (fn [p b] (box-contain-point? b p))
   ["@>" "circle" "circle"]   circle-contain-circle?
   ["<@" "circle" "circle"]   (fn [a b] (circle-contain-circle? b a))
   ["@>" "circle" "point"]    circle-contain-point?
   ["<@" "point" "circle"]    (fn [p c] (circle-contain-point? c p))
   ["<@" "point" "line"]      (fn [p l] (line-contain-point? l p))
   ["<@" "point" "lseg"]      (fn [p s] (lseg-contain-point? s p))
   ["<@" "lseg" "line"]       lseg-in-line?
   ["<@" "lseg" "box"]        lseg-in-box?
   ["@>" "polygon" "point"]   poly-contain-point?
   ["<@" "point" "polygon"]   (fn [p poly] (poly-contain-point? poly p))
   ["@>" "path" "point"]      path-contain-point?
   ["<@" "point" "path"]      (fn [p path] (path-contain-point? path p))})

(def ^:private spatial-ops-unimplemented
  "The pairs PostgreSQL has and this does not. Named so they refuse
   rather than answering `false`, which is what every one of the 21
   used to do."
  #{["&&" "polygon" "polygon"] ["@>" "polygon" "polygon"]
    ["<@" "polygon" "polygon"]})

(defn spatial-op-known?
  "Is `[op left-type right-type]` an operator PostgreSQL has for these
   geometric types -- whether or not we implement it?"
  [op lt rt]
  (or (contains? spatial-ops [op lt rt])
      (contains? spatial-ops-unimplemented [op lt rt])))

(defn spatial-op
  "The implementation of `[op lt rt]`, or nil when PostgreSQL has the
   operator and this does not."
  [op lt rt]
  (get spatial-ops [op lt rt]))
