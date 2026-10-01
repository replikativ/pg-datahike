(ns datahike.pg.mac
  "The MAC half of PostgreSQL's network-address family: `macaddr`
   (EUI-48, 6 bytes) and `macaddr8` (EUI-64, 8 bytes), from `mac.c` and
   `mac8.c`.

   Neither could be spelled at all. Their `pg_type` rows were generated
   from pg_type.dat, but nothing mapped the NAMES to them, so
   `'08:00:2b:01:02:03'::macaddr` was `type \"macaddr\" does not exist`
   and so was a column of one -- taking the rest of the file with it.

   A value is held as its canonical text, as the geometric family is.
   Here that costs nothing at all: the canonical form is fixed-width
   lower-case hex, so lexicographic text order IS `macaddr_cmp`'s
   unsigned byte order, and text equality is byte equality. No separate
   comparator is needed.

   The two input grammars are NOT the same, which is the thing to get
   right. `macaddr_in` is seven fixed `sscanf` patterns -- the groups
   may be 2, 3 or 6 digits wide depending on the pattern. `macaddr8_in`
   is a scanner: hex pairs with ONE separator character used
   consistently, 6 or 8 of them, and 6 means an EUI-48 that becomes
   EUI-64 by inserting ff:fe after the third byte."
  (:require [clojure.string :as str]
            [datahike.pg.errors :as errors]))

(defn- bad!
  [type-name ^String input]
  (throw (errors/pg-error :invalid-text-representation
                          {:type type-name :value input})))

(defn- render
  "Canonical output: lower-case hex pairs joined by colons
   (`macaddr_out` / `macaddr8_out`)."
  [bytes]
  (str/join ":" (map #(format "%02x" (bit-and (long %) 0xff)) bytes)))

;; ----------------------------------------------------------------------------
;; macaddr — seven fixed patterns
;; ----------------------------------------------------------------------------

(def ^:private macaddr-patterns
  "`macaddr_in`'s `sscanf` formats, in its order. Each is a regex whose
   groups are the six bytes. The group WIDTHS differ between patterns --
   `%x:%x:...` takes any width, `%2x%2x%2x:%2x%2x%2x` takes exactly two
   -- and the separator may not be mixed, which is why this is a list of
   whole-string patterns rather than one split."
  [#"(?i)^([0-9a-f]{1,2}):([0-9a-f]{1,2}):([0-9a-f]{1,2}):([0-9a-f]{1,2}):([0-9a-f]{1,2}):([0-9a-f]{1,2})$"
   #"(?i)^([0-9a-f]{1,2})-([0-9a-f]{1,2})-([0-9a-f]{1,2})-([0-9a-f]{1,2})-([0-9a-f]{1,2})-([0-9a-f]{1,2})$"
   #"(?i)^([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2}):([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$"
   #"(?i)^([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})-([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$"
   #"(?i)^([0-9a-f]{2})([0-9a-f]{2})\.([0-9a-f]{2})([0-9a-f]{2})\.([0-9a-f]{2})([0-9a-f]{2})$"
   #"(?i)^([0-9a-f]{2})([0-9a-f]{2})-([0-9a-f]{2})([0-9a-f]{2})-([0-9a-f]{2})([0-9a-f]{2})$"
   #"(?i)^([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$"])

(defn macaddr-bytes
  "The six bytes of a `macaddr`, or nil when the text is not one."
  [^String input]
  (let [t (str/trim (str input))]
    (some (fn [re]
            (when-let [m (re-matches re t)]
              (mapv #(Integer/parseInt % 16) (rest m))))
          macaddr-patterns)))

(defn macaddr-in
  "`macaddr_in`: canonical text, or an `invalid input syntax` error."
  [^String input]
  (if-let [bs (macaddr-bytes input)]
    (render bs)
    (bad! "macaddr" (str/trim (str input)))))

;; ----------------------------------------------------------------------------
;; macaddr8 — a scanner with one consistent separator
;; ----------------------------------------------------------------------------

(defn macaddr8-bytes
  "The eight bytes of a `macaddr8`, or nil when the text is not one.

   `macaddr8_in` is a scanner, not a set of patterns: it reads hex PAIRS
   and allows ONE separator character -- `:`, `-` or `.` -- after any
   pair, used consistently. So the groups may be any even width, which
   is how `08002b:0102030405` and `0800.2b01.0203.0405` are the same
   address, and why mixing `:` with `-` is refused.

   Six bytes is an EUI-48, which becomes EUI-64 by inserting `ff:fe`
   after the third -- the conversion that makes
   `'08:00:2b:01:02:03'::macaddr8` into `08:00:2b:ff:fe:01:02:03`
   rather than a left-padded eight."
  [^String input]
  (let [t (str/trim (str input))
        seps (filter #(str/includes? t (str %)) [\: \- \.])
        parts (if (seq seps)
                ;; One separator only. A second kind stays inside a part
                ;; and fails the hex test below, which is the rejection
                ;; PostgreSQL makes.
                (str/split t (re-pattern (java.util.regex.Pattern/quote
                                          (str (first seps)))) -1)
                [t])
        hex (apply str parts)]
    (when (and (every? #(and (even? (count %))
                             (pos? (count %))
                             (re-matches #"(?i)[0-9a-f]+" %))
                       parts)
               (contains? #{12 16} (count hex)))
      (let [bs (mapv #(Integer/parseInt (apply str %) 16) (partition 2 hex))]
        (if (= 6 (count bs))
          (vec (concat (subvec bs 0 3) [0xff 0xfe] (subvec bs 3 6)))
          bs)))))

(defn macaddr8-in
  "`macaddr8_in`: canonical text, or an `invalid input syntax` error."
  [^String input]
  (if-let [bs (macaddr8-bytes input)]
    (render bs)
    (bad! "macaddr8" (str/trim (str input)))))

;; ----------------------------------------------------------------------------
;; The family, for the cast / INSERT / DDL paths
;; ----------------------------------------------------------------------------

(def mac-type-names #{"macaddr" "macaddr8"})

(defn mac-type? [type-name] (contains? mac-type-names type-name))

(defn mac-in
  "Input for whichever of the two `type-name` is."
  [type-name v]
  (cond
    (nil? v) nil
    (= :__null__ v) :__null__
    (= "macaddr" type-name) (macaddr-in v)
    (= "macaddr8" type-name) (macaddr8-in v)
    :else v))

;; ----------------------------------------------------------------------------
;; Functions and operators
;; ----------------------------------------------------------------------------

(defn mac-trunc
  "`trunc(macaddr)` zeroes the last THREE bytes and `trunc(macaddr8)`
   the last FIVE -- in both cases everything after the 24-bit OUI."
  [v]
  (when (some? v)
    (let [bs (or (macaddr-bytes v) (macaddr8-bytes v))]
      (when bs
        (render (concat (take 3 bs) (repeat (- (count bs) 3) 0)))))))

(defn mac8-set7bit
  "`macaddr8_set7bit`: set the 7th bit of the first byte, which turns a
   locally-administered EUI-64 into the modified form IPv6 wants."
  [v]
  (when (some? v)
    (when-let [bs (macaddr8-bytes v)]
      (render (cons (bit-or (long (first bs)) 0x02) (rest bs))))))

(defn mac8->mac
  "`macaddr8::macaddr`, which is only defined when the 4th and 5th bytes
   are ff and fe -- i.e. when the EUI-64 was made from an EUI-48. Any
   other address would lose information, so PostgreSQL refuses it with
   a hint saying exactly that."
  [v]
  (when (some? v)
    (if-let [bs (macaddr8-bytes v)]
      (if (and (= 0xff (long (nth bs 3))) (= 0xfe (long (nth bs 4))))
        (render (concat (take 3 bs) (drop 5 bs)))
        (throw (ex-info "macaddr8 data out of range to convert to macaddr"
                        {:sqlstate "22003"
                         :error :numeric-value-out-of-range
                         :hint (str "Only addresses that have FF and FE as values "
                                    "in the 4th and 5th bytes from the left, for "
                                    "example xx:xx:xx:ff:fe:xx:xx:xx, are eligible "
                                    "to be converted from macaddr8 to macaddr.")})))
      (bad! "macaddr8" (str v)))))

(defn mac->mac8
  "`macaddr::macaddr8`, the EUI-48 to EUI-64 widening."
  [v]
  (when (some? v)
    (if-let [bs (macaddr-bytes v)]
      (render (concat (take 3 bs) [0xff 0xfe] (drop 3 bs)))
      (bad! "macaddr" (str v)))))

(defn- bitwise
  [f a b]
  (when (and (some? a) (some? b))
    (let [xs (or (macaddr-bytes a) (macaddr8-bytes a))
          ys (or (macaddr-bytes b) (macaddr8-bytes b))]
      (when (and xs ys (= (count xs) (count ys)))
        (render (map #(bit-and (f (long %1) (long %2)) 0xff) xs ys))))))

(defn mac-and [a b] (bitwise bit-and a b))
(defn mac-or  [a b] (bitwise bit-or a b))

(defn mac-not
  "`~macaddr`, the ones' complement of every byte."
  [v]
  (when (some? v)
    (when-let [bs (or (macaddr-bytes v) (macaddr8-bytes v))]
      (render (map #(bit-and (bit-not (long %)) 0xff) bs)))))
