(ns datahike.pg.interval.arith
  "Interval arithmetic: the `oprcode` functions behind `timestamp ±
   interval`, `interval ± interval`, `-interval`, `interval * n` and
   `interval / n`.

   THE MONTH/DAY/MICROSECOND SPLIT IS NOT AN IMPLEMENTATION DETAIL. An
   interval's three fields are added to a timestamp in three separate
   stages, in a fixed order, and each stage has different semantics
   (`timestamp_pl_interval`, timestamp.c:3070-3180):

     MONTHS go through the broken-down time, with year/month carry and
       then a CLAMP of the day to the target month's length. This is
       what makes `'2001-01-31' + '1 mon'` February 28 rather than
       March 3.
     DAYS go through `date2j`/`j2date` -- pure Julian-day arithmetic,
       no clamping, so `'2001-01-31' + '30 days'` is March 2.
     MICROSECONDS are added to the final value.

   Collapsing any of this into \"a month is 30 days\" gives the wrong
   answer for most of the calendar. The 30-day equivalence belongs to
   `interval_cmp_value`, which is about COMPARING two intervals, and
   nowhere else."
  (:require [datahike.pg.datetime.decode :as dt-dec]
            [datahike.pg.errors :as errors]
            [datahike.pg.interval.core :as ic]
            [datahike.pg.types :as types]))

(def ^:const int32-max 2147483647)
(def ^:const int32-min -2147483648)
(def ^:const usecs-per-day 86400000000)
(def ^:const usecs-per-sec 1000000)
(def ^:const months-per-year 12)

(defn- out-of-range!
  ([] (out-of-range! "timestamp out of range"))
  ([msg] (throw (errors/pg-error :datetime-field-overflow {:message msg}))))

(defn- check-i32 ^long [^long v]
  (when (or (> v int32-max) (< v int32-min)) (out-of-range! "interval out of range"))
  v)

;; ---------------------------------------------------------------------------
;; interval ± interval, -interval
;; ---------------------------------------------------------------------------

(defn negate
  "`interval_um_internal`: negate all three fields."
  [iv]
  (if (types/infinite-datetime iv)
    (if (= :neg (types/infinite-datetime iv)) types/pos-infinity types/neg-infinity)
    (ic/interval (check-i32 (- (ic/months iv)))
                 (check-i32 (- (ic/days iv)))
                 (- (ic/micros iv)))))

(defn- infinite-pair
  "The infinity rules shared by `interval_pl` and `timestamp_pl_interval`:
   two infinities of OPPOSITE sign are an error, and otherwise the
   infinity wins. Returns `[:done v]`, or nil when both are finite."
  [a b]
  (let [ia (types/infinite-datetime a) ib (types/infinite-datetime b)]
    (cond
      (and ia ib) (if (= ia ib) [:done a] (out-of-range! "interval out of range"))
      ia [:done a]
      ib [:done b]
      :else nil)))

(defn add
  "`interval_pl`: field-wise addition."
  [a b]
  (or (second (infinite-pair a b))
      (let [x a y b]
        (ic/interval (check-i32 (+ (ic/months x) (ic/months y)))
                     (check-i32 (+ (ic/days x) (ic/days y)))
                     (+ (ic/micros x) (ic/micros y))))))

(defn subtract
  "`interval_mi`. Not `add` of the negation in the C, but the same
   result for every finite pair; the infinity handling differs only in
   which message you get, and both are `interval out of range`."
  [a b]
  (if (types/infinite-datetime b)
    (add a (negate b))
    (or (second (infinite-pair a b))
        (let [x a y b]
          (ic/interval (check-i32 (- (ic/months x) (ic/months y)))
                       (check-i32 (- (ic/days x) (ic/days y)))
                       (- (ic/micros x) (ic/micros y)))))))

(defn- tsround
  "`TSROUND`: round to microsecond precision."
  ^double [^double v]
  (/ (Math/rint (* v 1000000.0)) 1000000.0))

(defn multiply
  "`interval_mul`. The fractional parts cascade DOWNWARD -- a fraction
   of a month becomes days (at 30 per month), and a fraction of a day
   becomes seconds -- which is why `'1 mon' * 0.5` is 15 days and not
   half a month."
  [iv ^double factor]
  (cond
    (Double/isNaN factor) (out-of-range! "interval out of range")
    (types/infinite-datetime iv)
    (cond (zero? factor) (out-of-range! "interval out of range")
          (neg? factor) (negate iv)
          :else iv)
    :else
    (let [x iv
          om (ic/months x) od (ic/days x)
          m (let [d (* om factor)]
              (when (or (Double/isNaN d) (> (Math/abs d) 2.147483647E9))
                (out-of-range! "interval out of range"))
              (long d))
          dd (let [d (* od factor)]
               (when (or (Double/isNaN d) (> (Math/abs d) 2.147483647E9))
                 (out-of-range! "interval out of range"))
               (long d))
          month-rem-days (tsround (* (- (* om factor) m) 30.0))
          sec-rem (tsround (* (+ (- (* od factor) dd)
                                 (- month-rem-days (long month-rem-days)))
                              86400.0))
          [dd sec-rem] (if (>= (Math/abs sec-rem) 86400.0)
                         [(+ dd (long (/ sec-rem 86400.0)))
                          (- sec-rem (* (long (/ sec-rem 86400.0)) 86400.0))]
                         [dd sec-rem])
          dd (check-i32 (+ dd (long month-rem-days)))
          micros (+ (Math/round (* (ic/micros x) factor))
                    (Math/round (* sec-rem 1000000.0)))]
      (ic/interval (check-i32 m) dd micros))))

(defn divide
  "`interval_div`. Division by zero is 22012, not an interval error."
  [iv ^double factor]
  (when (zero? factor)
    (throw (errors/pg-error :division-by-zero {:message "division by zero"})))
  (multiply iv (/ 1.0 factor)))

;; ---------------------------------------------------------------------------
;; timestamp ± interval
;; ---------------------------------------------------------------------------

(defn- clamp-day
  "The day CLAMP after a month shift: if the day is past the end of the
   target month, it becomes the last day of that month. This single
   line is the whole difference between adding a month and adding 30
   days."
  [^long year ^long mon ^long mday]
  (min mday (dt-dec/days-in-month year mon)))

(defn- add-months
  "The MONTH stage: year/month carry, then the clamp."
  [{:keys [year mon mday] :as tm} ^long months]
  (let [m (check-i32 (+ mon months))
        [year m] (cond
                   (> m months-per-year)
                   [(+ year (quot (dec m) months-per-year))
                    (inc (mod (dec m) months-per-year))]
                   (< m 1)
                   [(+ year (dec (quot m months-per-year)))
                    (+ (mod m months-per-year) months-per-year)]
                   :else [year m])]
    (assoc tm :year year :mon m :mday (clamp-day year m mday))))

(defn- add-days
  "The DAY stage: Julian-day arithmetic, no clamping."
  [{:keys [year mon mday] :as tm} ^long days]
  (let [j (+ (dt-dec/date2j year mon mday) days)]
    (when (neg? j) (out-of-range!))
    (let [[y m d] (dt-dec/j2date j)]
      (assoc tm :year y :mon m :mday d))))

(defn timestamp-plus
  "`timestamp_pl_interval`. `tm->ts` and `ts->tm` are the caller's
   conversions, so this works for whichever temporal carrier the call
   site holds.

   An infinite interval makes the result infinite, EXCEPT against a
   timestamp infinite the other way, which is an error. An infinite
   TIMESTAMP with a finite interval is returned unchanged."
  [ts iv {:keys [ts->fields fields->ts]}]
  (let [its (types/infinite-datetime ts)
        iiv (types/infinite-datetime iv)]
    (cond
      (and iiv its) (if (= iiv its) ts (out-of-range!))
      iiv (if (= :neg iiv) types/neg-infinity types/pos-infinity)
      its ts
      :else
      (let [x iv
            tm (ts->fields ts)
            tm (if (zero? (ic/months x)) tm (add-months tm (ic/months x)))
            tm (if (zero? (ic/days x)) tm (add-days tm (ic/days x)))]
        (fields->ts tm (ic/micros x))))))

(defn timestamp-minus
  "`timestamp_mi_interval`: plus the negation. The C negates the
   interval and calls the same code, and so does this -- which is why
   `'2001-03-31' - '1 mon'` clamps to February 28 exactly as the
   addition would."
  [ts iv conv]
  (timestamp-plus ts (negate iv) conv))

;; ---------------------------------------------------------------------------
;; time ± interval
;; ---------------------------------------------------------------------------

(defn time-plus
  "`time_pl_interval` (date.c:1745-1762): the microseconds only, WRAPPED
   into a single day. The months and days of the interval are ignored
   entirely -- a time has nowhere to put them -- so
   `'12:00'::time + '1 mon'` is 12:00.

   An infinite interval is an error here, with its own message."
  ^long [^long usec-of-day iv]
  (when (types/infinite-datetime iv)
    (throw (errors/pg-error :datetime-field-overflow
                            {:message "cannot add infinite interval to time"})))
  (let [t (+ usec-of-day (ic/micros iv))
        t (- t (* (quot t usecs-per-day) usecs-per-day))]
    (if (neg? t) (+ t usecs-per-day) t)))

(defn time-minus
  "`time_mi_interval`."
  ^long [^long usec-of-day iv]
  (time-plus usec-of-day (negate iv)))
