(ns datahike.pg.interval.core
  "The interval CARRIER, and `interval_out`.

   A `deftype`, deliberately, and not a `defrecord`. The consolidation
   plan warns that \"a record's structural `=`/hash would split GROUP BY
   and joins\", and that is true -- but it is a reason to choose the
   carrier's equality, not a reason to avoid a carrier. PostgreSQL
   compares `'1 mon'` and `'30 days'` as EQUAL (`interval_cmp_value`:
   a month is 30 days, a day is 24 hours) while PRINTING them
   differently, so:

     structural equality splits two values PostgreSQL groups;
     canonical TEXT cannot be the key either, for the same reason.

   `clojure.core/=` routes a non-collection, non-Number object to
   `.equals`, and `clojure.core/hash` routes a non-`IHashEq`,
   non-Number, non-String object to `.hashCode`. So implementing those
   two over `interval_cmp_value` makes the EXISTING machinery correct,
   including the parts no comparator can reach: `GROUP BY` is
   datahike's own `group-by` over the non-aggregate `:find` elements,
   and `DISTINCT`, `clojure.set/intersection`, `contains?` on a set and
   datahike's hash join are all `clojure.core` calls. None of them
   takes a comparator; all of them are served by `.hashCode`.

   `Comparable` covers ORDER BY, the comparison operators, BETWEEN and
   min/max, through `fns/order-cmp`'s existing `:else (compare a b)` --
   so that function needs no interval branch at all. The same route
   `bits.clj`'s `PgBit` takes.

   THE SPAN NEEDS MORE THAN 64 BITS. `month * 30 + day` can reach
   6.4e10 days, and multiplying that by 86,400,000,000 µs overflows
   int64 -- which is why the C uses INT128. `compareTo` therefore works
   in `BigInteger`. `hashCode` does NOT: `interval_hash` deliberately
   narrows the INT128 to its low 64 bits (`int128_to_int64`) before
   hashing, so two spans differing by exactly 2^64 µs hash together in
   PostgreSQL, and matching that is what keeps our hashing consistent
   with a real server's.

   `toString` is NOT the SQL rendering. Output goes through
   `types/->pg-text`, which is the single funnel for `::text`, array
   elements, record fields, COPY, pg_dump and `to_jsonb`."
  (:require [clojure.string :as str]
            [datahike.pg.datetime.lex :as lex]
            [datahike.pg.errors :as errors]
            [datahike.pg.interval.decode :as decode]
            [datahike.pg.types :as types])
  (:import [java.math BigInteger]))

(def ^:const months-per-year 12)
(def ^:const usecs-per-day 86400000000)
(def ^:const usecs-per-hour 3600000000)
(def ^:const usecs-per-minute 60000000)
(def ^:const usecs-per-sec 1000000)

(def ^:private big-usecs-per-day (BigInteger/valueOf usecs-per-day))
(def ^:private two64 (.shiftLeft BigInteger/ONE 64))
(def ^:private two63 (.shiftLeft BigInteger/ONE 63))

(defn cmp-span
  "`interval_cmp_value` (timestamp.c): the INT128 span, as a
   `BigInteger`. A month is 30 days and a day is 24 hours -- the
   equivalence that makes `'1 mon' = '30 days'`."
  ^BigInteger [^long months ^long days ^long micros]
  (let [d (+ (* months 30) days)]
    (.add (BigInteger/valueOf micros)
          (.multiply (BigInteger/valueOf d) big-usecs-per-day))))

(defn- span->low64
  "`int128_to_int64`: the low 64 bits, interpreted as signed. This is
   what `interval_hash` hashes, so two spans differing by 2^64
   microseconds hash together -- in PostgreSQL and here."
  ^long [^BigInteger span]
  (let [m (.mod span two64)]
    (.longValue (if (>= (.compareTo m two63) 0) (.subtract m two64) m))))

(declare ->pg-text-postgres)

(deftype PgInterval [^int months ^int days ^long micros]
  Object
  (equals [_ other]
    (and (instance? PgInterval other)
         (zero? (.compareTo (cmp-span months days micros)
                            (cmp-span (.-months ^PgInterval other)
                                      (.-days ^PgInterval other)
                                      (.-micros ^PgInterval other))))))
  (hashCode [_]
    (Long/hashCode (span->low64 (cmp-span months days micros))))
  ;; For a REPL and for error messages only. The SQL rendering is
  ;; `types/->pg-text`; see the namespace docstring.
  (toString [this] (->pg-text-postgres this))

  Comparable
  (compareTo [_ other]
    (.compareTo (cmp-span months days micros)
                (cmp-span (.-months ^PgInterval other)
                          (.-days ^PgInterval other)
                          (.-micros ^PgInterval other)))))

(defn interval
  "A carrier from the decoder's three fields."
  ^PgInterval [months days micros]
  (PgInterval. (int months) (int days) (long micros)))

(defn months
  "The carrier's month field.

   An ACCESSOR rather than direct `.-months` field access with a type
   hint, because a hint is resolved at compile time and a `deftype`
   reload mints a NEW class: a namespace hinted against the old one
   then fails with the memorable
   `PgInterval cannot be cast to class PgInterval`. Going through
   functions in this namespace means a reload of it is enough."
  ^long [^PgInterval iv] (.-months iv))

(defn days ^long [^PgInterval iv] (.-days iv))
(defn micros ^long [^PgInterval iv] (.-micros iv))

(defn interval? [v] (instance? PgInterval v))

(defn fields
  "`interval2itm` (timestamp.c): the carrier as the seven output
   fields. The divisions are TRUNCATING, so a negative interval keeps
   each field's own sign -- which is what makes `-1 days +02:03:04`
   printable at all."
  [^PgInterval iv]
  (let [m (.-months iv)
        t (.-micros iv)
        h (quot t usecs-per-hour)
        t1 (- t (* h usecs-per-hour))
        mi (quot t1 usecs-per-minute)
        t2 (- t1 (* mi usecs-per-minute))
        s (quot t2 usecs-per-sec)]
    {:year (quot m months-per-year) :mon (rem m months-per-year)
     :mday (.-days iv) :hour h :min mi :sec s :usec (- t2 (* s usecs-per-sec))}))

;; ---------------------------------------------------------------------------
;; EncodeInterval, INTSTYLE_POSTGRES (datetime.c:4703-4720)
;; ---------------------------------------------------------------------------

(defn- append-seconds
  "`AppendSeconds` with MAX_INTERVAL_PRECISION: `ss` zero-padded to two
   when `fillzeros`, then the fraction with trailing zeros stripped and
   the point dropped when nothing is left.

   IT EMITS NO SIGN. The C uses `abs(sec)` and `abs(fsec)` throughout
   (datetime.c:4420-4455) because the sign is already carried by
   whatever precedes it -- the `hh:mm` prefix in the postgres style, an
   explicit `-` in the ISO one. Signing here produced
   `1 day -02:03:-04`."
  [^long sec ^long usec fillzeros?]
  (let [a (Math/abs sec)
        base (if fillzeros? (format "%02d" a) (str a))
        frac (when-not (zero? usec)
               (let [f (str/replace (format "%06d" (Math/abs usec)) #"0+$" "")]
                 (when (seq f) (str "." f))))]
    (str base frac)))

(defn- add-postgres-int-part
  "`AddPostgresIntPart` (datetime.c:4535-4551).

   The accumulator keys are `:is-zero` and `:is-before`, not `:zero?`
   and `:before?`: destructuring the latter SHADOWS `clojure.core/zero?`
   and the next `(zero? hour)` call becomes a Boolean invocation.

   `is_before` is the subtle one, and the C's own comment calls it
   bizarre: each NONZERO field sets it for the NEXT field only, and a
   positive field that follows a negative one is printed with an
   explicit `+`. That is what produces `-1 days +02:03:04` rather than
   `-1 days 02:03:04`, and getting it wrong is a silent difference in
   pg_dump output."
  [acc ^long value ^String units]
  (if (zero? value)
    acc
    (let [{:keys [s is-zero is-before]} acc]
      {:s (str s (when-not is-zero " ")
               (when (and is-before (pos? value)) "+")
               value " " units (when (not= value 1) "s"))
       :is-zero false :is-before (neg? value)})))

(defn ->pg-text-postgres
  "`interval_out` under IntervalStyle = postgres, which is the only
   style the server reports (`show IntervalStyle` answers `postgres`
   unconditionally today). The other three are in the backlog with the
   GUC."
  ^String [^PgInterval iv]
  (let [{:keys [year mon mday hour min sec usec]} (fields iv)
        acc (-> {:s "" :is-zero true :is-before false}
                (add-postgres-int-part year "year")
                (add-postgres-int-part mon "mon")
                (add-postgres-int-part mday "day"))
        {:keys [s is-zero is-before]} acc]
    (if (or is-zero (not (zero? hour)) (not (zero? min))
            (not (zero? sec)) (not (zero? usec)))
      (let [minus? (or (neg? hour) (neg? min) (neg? sec) (neg? usec))]
        (str s (when-not is-zero " ")
             (cond minus? "-" is-before "+" :else "")
             (format "%02d" (Math/abs (long hour))) ":"
             (format "%02d" (Math/abs (long min))) ":"
             (append-seconds sec usec true)))
      s)))

;; ---------------------------------------------------------------------------
;; interval_in (timestamp.c:900-960)
;; ---------------------------------------------------------------------------

(defn interval-in
  "`interval_in`. Text to a `PgInterval`, or the infinity sentinels.

   THE FALLBACK ORDER IS EXACT and is the part worth getting right:
   `ParseDateTime` then `DecodeInterval`, and `DecodeISO8601Interval`
   is tried ONLY when that returned DTERR_BAD_FORMAT specifically --
   not on an overflow. So an input the ordinary decoder rejects as out
   of range must NOT be retried as ISO, or `'2147483648 months'` would
   fall through and be refused with the wrong error.

   And FIELD_OVERFLOW is REMAPPED to INTERVAL_OVERFLOW, SQLSTATE
   22015 -- `interval field value out of range` -- not the 22008 the
   datetime types raise (timestamp.c:941-943)."
  ([^String s] (interval-in s nil))
  ([^String s range]
   (let [range (or range decode/full-range)
         attempt
         (fn [] (try
                  [:ok (decode/decode-interval
                        (lex/tokenize s (:interval lex/buflen-for)) range false)]
                  (catch clojure.lang.ExceptionInfo e
                    (if-let [k (::lex/dterr (ex-data e))] [:err k] (throw e)))))
         [tag v] (attempt)
         [tag v] (if (= v :bad-format)
                   (try [:ok (decode/decode-iso8601-interval s)]
                        (catch clojure.lang.ExceptionInfo e
                          (if-let [k (::lex/dterr (ex-data e))] [:err k] (throw e))))
                   [tag v])]
     (if (= tag :err)
       (throw (errors/pg-error
               (case v
                 (:field-overflow :interval-overflow) :interval-field-overflow
                 :invalid-datetime-format)
               {:type "interval" :value s}))
       (case (:dtype v)
         :delta (interval (:months v) (:days v) (:micros v))
         :late types/pos-infinity
         :early types/neg-infinity)))))

;; ---------------------------------------------------------------------------
;; justify_hours / justify_days / justify_interval (timestamp.c:2952-3060)
;;
;; `justify_interval` is also the CANONICAL FORM for the
;; `interval_cmp` equivalence class -- cmp-equal intervals justify to
;; the same three fields, checked over eight pairs on the oracle. That
;; is why the differential corpus projects values through it: `::text`
;; is spelling, and PostgreSQL prints `1 mon` and `30 days`
;; differently while comparing them equal.
;; ---------------------------------------------------------------------------

(defn- tmodulo
  "`TMODULO(t, q, u)`: q gets the quotient, t the remainder, both
   TRUNCATED toward zero. Returns `[remainder quotient]`."
  [^long t ^long u]
  (let [q (quot t u)] [(- t (* q u)) q]))

(defn justify-hours
  "`interval_justify_hours`: whole days out of the time field, then a
   sign correction so day and time agree in sign."
  ^PgInterval [^PgInterval iv]
  (let [[t wholeday] (tmodulo (.-micros iv) usecs-per-day)
        d (+ (.-days iv) wholeday)
        [d t] (cond
                (and (pos? d) (neg? t)) [(dec d) (+ t usecs-per-day)]
                (and (neg? d) (pos? t)) [(inc d) (- t usecs-per-day)]
                :else [d t])]
    (interval (.-months iv) d t)))

(defn justify-days
  "`interval_justify_days`: whole months out of the day field, with the
   same sign correction."
  ^PgInterval [^PgInterval iv]
  (let [wholemonth (quot (.-days iv) 30)
        d (- (.-days iv) (* wholemonth 30))
        m (+ (.-months iv) wholemonth)
        [m d] (cond
                (and (pos? m) (neg? d)) [(dec m) (+ d 30)]
                (and (neg? m) (pos? d)) [(inc m) (- d 30)]
                :else [m d])]
    (interval m d (.-micros iv))))

(defn justify-interval
  "`interval_justify_interval`. Not simply justify-days after
   justify-hours: the FIRST month extraction runs only when day and
   time already agree in sign, and the final sign correction looks at
   the time field too -- `month > 0 && (day < 0 || (day == 0 && time <
   0))`. Composing the other two gives a different answer for a mixed
   interval."
  ^PgInterval [^PgInterval iv]
  (let [m0 (.-months iv) d0 (.-days iv) t0 (.-micros iv)
        [m1 d1] (if (or (and (pos? d0) (pos? t0)) (and (neg? d0) (neg? t0)))
                  (let [wm (quot d0 30)] [(+ m0 wm) (- d0 (* wm 30))])
                  [m0 d0])
        [t2 wholeday] (tmodulo t0 usecs-per-day)
        d2 (+ d1 wholeday)
        wm2 (quot d2 30)
        d3 (- d2 (* wm2 30))
        m3 (+ m1 wm2)
        [m4 d4] (cond
                  (and (pos? m3) (or (neg? d3) (and (zero? d3) (neg? t2))))
                  [(dec m3) (+ d3 30)]
                  (and (neg? m3) (or (pos? d3) (and (zero? d3) (pos? t2))))
                  [(inc m3) (- d3 30)]
                  :else [m3 d3])
        ;; A SECOND sign correction, between day and time, which the
        ;; month/day one above does not cover. Without it
        ;; `justify_interval('-1 2:03:04')` came out as
        ;; `-1 days +02:03:04` where the oracle gives `-21:56:56`: the
        ;; month correction cannot fire when there are no months, and a
        ;; day and a time of opposite sign are left as they are.
        [d5 t5] (cond
                  (and (pos? d4) (neg? t2)) [(dec d4) (+ t2 usecs-per-day)]
                  (and (neg? d4) (pos? t2)) [(inc d4) (- t2 usecs-per-day)]
                  :else [d4 t2])]
    (interval m4 d5 t5)))
