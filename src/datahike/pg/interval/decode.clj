(ns datahike.pg.interval.decode
  "`DecodeInterval` (datetime.c:3364-3749) and the `Adjust*` helpers
   (560-670).

   It reads the SAME lexed fields the datetime decoders read -- it is a
   fourth consumer of `datetime/lex.clj`, with `buflen` 256 rather than
   153 or 129 -- and then interprets them by a different set of rules.

   THE LOOP RUNS RIGHT TO LEFT (datetime.c:3397). Units FOLLOW their
   values in an interval literal, so `'1 day'` is read as `day` and
   then `1`, with the unit setting `type` for the field about to be
   read. Nothing else in the datetime family works this way, and
   reversing it is not a detail: `parsing_unit_val`, AGO's
   last-field-only rule and `DTK_HOUR`'s hand-off all depend on the
   direction.

   The accumulator is `pg_itm_in` (timestamp.h:82): `{:usec :mday :mon
   :year}`, where YEAR is kept separate from MONTH during decoding and
   folded in only at the end by `itmin2interval`. Keeping them apart is
   what lets `AdjustYears` multiply by 10, 100 and 1000 for decade,
   century and millennium without losing the int32 range.

   OVERFLOW IS CHECKED AT EVERY STEP in the C, with
   `pg_add_s32_overflow` and friends, and each failure is
   DTERR_FIELD_OVERFLOW -- which `interval_in` then REMAPS to
   DTERR_INTERVAL_OVERFLOW, SQLSTATE 22015, not 22008
   (timestamp.c:932-941). Clojure longs do not overflow at int32, so
   the bounds are checked explicitly; a missing check is a silently
   wrapped value rather than an error."
  (:require [clojure.set :as set]
            [datahike.pg.datetime.decode :as dt-dec]
            [datahike.pg.datetime.lex :as lex]
            [datahike.pg.interval.tokens :as tokens]))

(def ^:const int32-max 2147483647)
(def ^:const int32-min -2147483648)
(def ^:const usecs-per-sec 1000000)
(def ^:const usecs-per-minute 60000000)
(def ^:const usecs-per-hour 3600000000)
(def ^:const usecs-per-day 86400000000)
(def ^:const months-per-year 12)
(def ^:const days-per-month 30)

(defn- overflow! [] (dt-dec/dterr :field-overflow))

(def empty-itm
  "`ClearPgItmIn`."
  {:usec 0 :mday 0 :mon 0 :year 0})

;; ---------------------------------------------------------------------------
;; The Adjust* helpers (datetime.c:536-670)
;;
;; Each returns the updated itm or throws. The C returns a bool and the
;; caller turns false into DTERR_FIELD_OVERFLOW at every single call
;; site, so throwing is the same control flow with one fewer thing to
;; forget.
;; ---------------------------------------------------------------------------

(defn- check-i32 ^long [^long v]
  (when (or (> v int32-max) (< v int32-min)) (overflow!))
  v)

(defn- add-usec [itm ^long d]
  (let [v (+' (:usec itm) d)]
    (when (or (> v Long/MAX_VALUE) (< v Long/MIN_VALUE)) (overflow!))
    (assoc itm :usec (long v))))

(defn adjust-fract-microseconds
  "`AdjustFractMicroseconds` (536-561). The rounding is explicit and
   is NOT `Math/round`: the C truncates to int64 and then nudges by one
   only when the leftover fraction exceeds a half in either direction,
   which rounds -0.5 and +0.5 DOWN in magnitude where `Math/round`
   would round +0.5 up."
  [itm ^double frac ^double scale]
  (if (zero? frac)
    itm
    (let [f (* frac scale)
          u (long f)
          rem (- f u)
          u (cond (> rem 0.5) (inc u) (< rem -0.5) (dec u) :else u)]
      (add-usec itm u))))

(defn adjust-fract-days
  "`AdjustFractDays` (564-578). The INTEGER part of the scaled fraction
   becomes whole days and only the remainder becomes microseconds."
  [itm ^double frac ^long scale]
  (if (zero? frac)
    itm
    (let [f (* frac scale)
          extra (long f)
          itm (assoc itm :mday (check-i32 (+ (:mday itm) extra)))]
      (adjust-fract-microseconds itm (- f extra) usecs-per-day))))

(defn adjust-fract-years
  "`AdjustFractYears` (581-588). `rint` -- round half to EVEN, which is
   `Math/rint`, not `Math/round`. `rint(0.5)` is 0 and `round(0.5)` is
   1, and the difference is reachable: `'0.5 mons'` would gain a month."
  [itm ^double frac ^long scale]
  (assoc itm :mon (check-i32 (+ (:mon itm)
                                (long (Math/rint (* frac scale months-per-year)))))))

(defn adjust-microseconds
  [itm ^long val ^double fval ^long scale]
  (-> itm
      (add-usec (let [v (*' val scale)]
                  (when (or (> v Long/MAX_VALUE) (< v Long/MIN_VALUE)) (overflow!))
                  (long v)))
      (adjust-fract-microseconds fval (double scale))))

(defn adjust-days [itm ^long val ^long scale]
  (check-i32 val)
  (assoc itm :mday (check-i32 (+ (:mday itm) (check-i32 (*' val scale))))))

(defn adjust-months [itm ^long val]
  (check-i32 val)
  (assoc itm :mon (check-i32 (+ (:mon itm) val))))

(defn adjust-years [itm ^long val ^long scale]
  (check-i32 val)
  (assoc itm :year (check-i32 (+ (:year itm) (check-i32 (*' val scale))))))

;; ---------------------------------------------------------------------------
;; The typmod range (datetime.c:3418-3474)
;; ---------------------------------------------------------------------------

(def time-fields
  "DTK_TIME_M, which a `:time` field claims whole."
  #{:hour :minute :second})

(def full-range
  "INTERVAL_FULL_RANGE. Every field allowed, which is what a bare
   `::interval` cast uses."
  #{:year :month :day :hour :minute :second})

(defn range-default-type
  "What a BARE number means, given the typmod `range`
   (datetime.c:3421-3474).

   This is the whole reason `interval '1' hour` is one hour and
   `'1'::interval` is one second. The switch is on the EXACT field set,
   not on its lowest member, and the arms are the ones the C lists --
   anything else falls to SECOND."
  [range]
  (condp = range
    #{:year} :year
    #{:month} :month
    #{:year :month} :month
    #{:day} :day
    #{:hour} :hour
    #{:day :hour} :hour
    #{:minute} :minute
    #{:hour :minute} :minute
    #{:day :hour :minute} :minute
    #{:second} :second
    #{:minute :second} :second
    #{:hour :minute :second} :second
    #{:day :hour :minute :second} :second
    :second))

;; ---------------------------------------------------------------------------
;; DecodeInterval
;; ---------------------------------------------------------------------------

(defn- decode-time-for-interval
  "`DecodeTimeForInterval` (datetime.c:2693-2711): `DecodeTimeCommon`,
   then the whole h/m/s collapsed into microseconds. Reuses the
   datetime port's `decode-time`, which IS DecodeTimeCommon."
  ^long [^String s]
  (let [{:keys [hour min sec usec]} (dt-dec/decode-time s)]
    (+' (*' (long hour) usecs-per-hour)
        (*' (long min) usecs-per-minute)
        (*' (long sec) usecs-per-sec)
        (long usec))))

(def ^:private unit-scale
  "The per-unit arm of the big switch (datetime.c:3542-3650), as data:
   `[kind scale tmask-field]`. `:usec` units go through
   `AdjustMicroseconds`, `:day` units through `AdjustDays` +
   `AdjustFractMicroseconds`, `:year` units through `AdjustYears` +
   `AdjustFractYears`."
  {:microsec   [:usec 1 :microsecond]
   :millisec   [:usec 1000 :millisecond]
   :second     [:usec usecs-per-sec :second]
   :minute     [:usec usecs-per-minute :minute]
   :hour       [:usec usecs-per-hour :hour]
   :day        [:day 1 :day]
   :week       [:week 7 :week]
   :month      [:month 1 :month]
   :year       [:year 1 :year]
   :decade     [:year 10 :decade]
   :century    [:year 100 :century]
   :millennium [:year 1000 :millennium]})

(defn- apply-unit
  "One value+fraction into the accumulator, under `type`. Returns
   `[itm tmask next-type]`; `next-type` differs from `type` only for
   DTK_HOUR, which sets DAY for the field about to be read -- that is
   what makes `'1 2:03:04'` one day and two hours (datetime.c:3621)."
  [itm type ^long val ^double fval]
  (if-let [[kind scale field] (get unit-scale type)]
    (case kind
      :usec [(adjust-microseconds itm val fval scale)
             ;; A fractional SECOND claims the whole second family, so
             ;; `'1.5 sec'` cannot be combined with a separate
             ;; millisecond field.
             (if (and (= type :second) (not (zero? fval)))
               #{:second :millisecond :microsecond}
               #{field})
             (if (= type :hour) :day type)]
      :day [(-> itm (adjust-days val 1) (adjust-fract-microseconds fval usecs-per-day))
            #{field} type]
      :week [(-> itm (adjust-days val scale) (adjust-fract-days fval scale))
             #{field} type]
      :month [(-> itm (adjust-months val) (adjust-fract-days fval days-per-month))
              #{field} type]
      :year [(-> itm (adjust-years val scale) (adjust-fract-years fval scale))
             #{field} type])
    ;; DTK_QUARTER and the three timezone units resolve in deltatktbl
    ;; and reach this `default`. See interval/tokens.clj.
    (dt-dec/dterr :bad-format)))

(defn- parse-value
  "The value half of a DTK_DATE/DTK_NUMBER field (datetime.c:3477-3517):
   an int64, then either a `-mm` SQL years-months tail, a `.frac`, or
   nothing.

   Returns `[val fval type']`. The years-months form OVERRIDES the
   type to MONTH, which is how `'1-2'` is a year and two months
   whatever the typmod said."
  [^String s type]
  (let [n (.length s)
        [v cp] (#'dt-dec/strtoint s 0)
        c (when (< cp n) (.charAt s cp))]
    (cond
      (= c \-)
      (let [[v2 cp2] (#'dt-dec/strtoint s (inc cp))]
        ;; `val2 < 0 || val2 >= MONTHS_PER_YEAR` is FIELD_OVERFLOW, not
        ;; a format error -- `'1-13'` is 22015, not 22007.
        (when (or (neg? v2) (>= v2 months-per-year)) (overflow!))
        (when (not= cp2 n) (dt-dec/dterr :bad-format))
        (let [v2 (if (= (.charAt s 0) \-) (- v2) v2)]
          [(+' (*' v months-per-year) v2) 0.0 :month]))

      (= c \.)
      (let [f (#'dt-dec/parse-fraction s cp)]
        [v (if (= (.charAt s 0) \-) (- f) f) type])

      (nil? c) [v 0.0 type]
      :else (dt-dec/dterr :bad-format))))

(defn decode-interval
  "`DecodeInterval`. Returns
   `{:dtype :delta|:late|:early :months :days :micros}`, or throws.

   `range` is the typmod field set, defaulting to every field.
   `sql-standard?` enables the `force_negative` pass, which only
   `IntervalStyle = SQL_STANDARD` turns on."
  ([fields] (decode-interval fields full-range false))
  ([fields range sql-standard?]
   (let [nf (count fields)
         fv (vec fields)
         ;; `force_negative` (datetime.c:3384-3395): a leading `-` on
         ;; the FIRST field, and only when no later field carries a
         ;; sign of its own. It makes every value negative, which is
         ;; how SQL_STANDARD reads `'-1 2:03:04'` as all-negative
         ;; where the postgres style reads a negative day and a
         ;; positive time.
         force-negative?
         (and sql-standard? (pos? nf)
              (= (.charAt ^String (:text (fv 0)) 0) \-)
              (not-any? #(#{\- \+} (.charAt ^String (:text %) 0)) (subvec fv 1)))]
     (loop [i (dec nf)
            itm empty-itm
            fmask #{}
            type :ignore-dtf
            dtype :delta
            unit-pending? false
            before? false]
       (if (neg? i)
         (do
           ;; Nothing claimed at all -- `'@'` and `'ago'` alone.
           (when (empty? fmask) (dt-dec/dterr :bad-format))
           ;; A unit with no value after it: `'1 day 2 day'` reaches
           ;; this, and so does a trailing bare unit.
           (when unit-pending? (dt-dec/dterr :bad-format))
           (let [itm (if before?
                       (do (when (or (= (:usec itm) Long/MIN_VALUE)
                                     (= (:mday itm) int32-min)
                                     (= (:mon itm) int32-min)
                                     (= (:year itm) int32-min))
                             (overflow!))
                           (-> itm (update :usec -) (update :mday -)
                               (update :mon -) (update :year -)))
                       itm)
                 total-months (+' (*' (long (:year itm)) months-per-year)
                                  (long (:mon itm)))]
             (if (= dtype :delta)
               (do (when (or (> total-months int32-max) (< total-months int32-min))
                     (overflow!))
                   {:dtype :delta :months (long total-months)
                    :days (:mday itm) :micros (:usec itm)})
               {:dtype dtype :months 0 :days 0 :micros 0})))

         (let [{:keys [text] :as f} (fv i)
               ftype (:type f)
               neg-time (fn [^long u]
                          (if (and force-negative? (pos? u)) (- u) u))
               [itm' tmask type' dtype' pending' before'']
               (case ftype
                 ;; `DecodeTimeCommon` sets `*tmask = DTK_TIME_M`, so a
                 ;; time field CLAIMS hour, minute and second. With an
                 ;; empty tmask `'24:00:00'` reached the end with
                 ;; nothing claimed and was refused as `fmask == 0`.
                 :time
                 (let [u (neg-time (decode-time-for-interval text))]
                   [(assoc itm :usec u) time-fields :day dtype false before?])

                 ;; A `+hh:mm` / `-hh:mm` field is a TIME here, not a
                 ;; zone -- but only when it has a colon. Without one
                 ;; it FALLS THROUGH to the number case, which is why
                 ;; `'-01:00'` is a time and `'+1'` is a value.
                 :tz
                 (if (and (.contains ^String (subs text 1) ":")
                          (try (decode-time-for-interval (subs text 1)) true
                               (catch clojure.lang.ExceptionInfo _ false)))
                   (let [u (decode-time-for-interval (subs text 1))
                         u (if (= (.charAt ^String text 0) \-)
                             (do (when (= u Long/MIN_VALUE) (overflow!)) (- u))
                             u)]
                     [(assoc itm :usec (neg-time u)) time-fields :day dtype
                      false before?])
                   ;; fall through to the number reading
                   (let [type (if (= type :ignore-dtf) (range-default-type range) type)
                         [v fv' t2] (parse-value text type)
                         [v fv'] (if force-negative?
                                   [(if (pos? v) (- v) v)
                                    (if (pos? fv') (- fv') fv')]
                                   [v fv'])
                         [itm2 tm t3] (apply-unit itm t2 v fv')]
                     [itm2 tm t3 dtype false before?]))

                 (:date :number)
                 (let [type (if (= type :ignore-dtf) (range-default-type range) type)
                       [v fv' t2] (parse-value text type)
                       [v fv'] (if force-negative?
                                 [(if (pos? v) (- v) v)
                                  (if (pos? fv') (- fv') fv')]
                                 [v fv'])
                       [itm2 tm t3] (apply-unit itm t2 v fv')]
                   [itm2 tm t3 dtype false before?])

                 (:string :special)
                 (do
                   ;; Two units in a row with no value between them.
                   (when unit-pending? (dt-dec/dterr :bad-format))
                   (let [[kind uval] (tokens/unit-or-special text)]
                     (case kind
                       :ignore-dtf ::skip
                       :units [itm #{} uval dtype true before?]
                       :ago (do (when (not= i (dec nf)) (dt-dec/dterr :bad-format))
                                [itm #{} type dtype false true])
                       :reserv
                       (do (when-not (#{:late :early} uval) (dt-dec/dterr :bad-format))
                           (when (not= i (dec nf)) (dt-dec/dterr :bad-format))
                           [itm (into full-range [:hour :minute :second])
                            type uval false before?])
                       (dt-dec/dterr :bad-format))))

                 (dt-dec/dterr :bad-format))]
           (if (= itm' ::skip)
             (recur (dec i) itm fmask type dtype unit-pending? before?)
             (do
               (when (seq (set/intersection fmask tmask))
                 (dt-dec/dterr :bad-format))
               (recur (dec i) itm' (into fmask tmask) type' dtype'
                      pending' before'')))))))))

;; ---------------------------------------------------------------------------
;; DecodeISO8601Interval (datetime.c:3829-4033)
;;
;; `interval_in` tries `DecodeInterval` FIRST and comes here only when
;; that returned DTERR_BAD_FORMAT specifically -- not on an overflow
;; (timestamp.c:932-941). So this is a fallback, and an input that
;; overflows the ordinary decoder must not be retried as ISO.
;; ---------------------------------------------------------------------------

(defn- parse-iso-number
  "`ParseISO8601Number` (3760-3790): a `strtod` with the integer and
   fractional parts split, and `floor` TOWARD ZERO -- `(int64) floor(v)`
   for positives and `(int64) -floor(-v)` for negatives, which is
   truncation, not `Math/floor`.

   Returns `[ipart fpart end-index]`."
  [^String s ^long i]
  (let [n (.length s)]
    (when-not (and (< i n)
                   (or (Character/isDigit (.charAt s i))
                       (= (.charAt s i) \-) (= (.charAt s i) \.)))
      (dt-dec/dterr :bad-format))
    ;; strtod's extent: an optional sign, digits, an optional point and
    ;; more digits. No exponent: PostgreSQL would accept one here, but
    ;; `P1e3D` is not reachable through the lexer, which never produces
    ;; a field containing `e` beside digits in this position.
    (let [j (if (#{\- \+} (.charAt s i)) (inc i) i)
          j (loop [k j] (if (and (< k n) (Character/isDigit (.charAt s k)))
                          (recur (inc k)) k))
          j (if (and (< j n) (= (.charAt s j) \.))
              (loop [k (inc j)] (if (and (< k n) (Character/isDigit (.charAt s k)))
                                  (recur (inc k)) k))
              j)]
      (when (= j i) (dt-dec/dterr :bad-format))
      (let [v (try (Double/parseDouble (subs s i j))
                   (catch NumberFormatException _ (dt-dec/dterr :bad-format)))]
        (when (or (Double/isNaN v) (< v -1.0e15) (> v 1.0e15))
          (overflow!))
        (let [ip (long (if (>= v 0) (Math/floor v) (- (Math/floor (- v)))))]
          [ip (- v ip) j])))))

(defn- iso-integer-width
  "`ISO8601IntegerWidth` (3793-3800): how many digits the field starts
   with, after an optional `-`. Width 8 before `T` and width 6 after it
   select the ALTERNATIVE basic formats, `P00010203` and `PT040506`."
  ^long [^String s ^long start]
  (let [n (.length s)
        i (if (and (< start n) (= (.charAt s start) \-)) (inc start) start)]
    (loop [k i] (if (and (< k n) (Character/isDigit (.charAt s k)))
                  (recur (inc k)) (- k i)))))

(defn- itm->interval
  "`itmin2interval` (timestamp.c): fold the separate YEAR field into
   months and hand back the three-field interval."
  [itm]
  (let [t (+' (*' (long (:year itm)) months-per-year) (long (:mon itm)))]
    (when (or (> t int32-max) (< t int32-min)) (overflow!))
    {:dtype :delta :months (long t) :days (:mday itm) :micros (:usec itm)}))

(defn decode-iso8601-interval
  "`DecodeISO8601Interval`. The designator format (`P1Y2M3DT4H5M6S`)
   and both alternative formats -- basic (`P00010203T040506`) and
   extended (`P0001-02-03T04:05:06`).

   Two documented departures from the spec, both in the C's own
   comment: a `W` field may coexist with other units, and decimals are
   allowed in fields other than the least significant.

   The C reaches the alternative formats by FALLTHROUGH -- `case 'T'`
   and `case '\\0'` fall into `case '-'` when the width-8 test fails --
   so the three are not separate branches but one entered at different
   points. Each branch here yields a uniform `[:return m]` or
   `[:recur …]` step, because the fallthrough targets cannot be inner
   functions: `recur` inside one would bind to the function, not the
   loop."
  [^String input]
  (let [s input
        n (.length s)]
    (when (or (< n 2) (not= (.charAt s 0) \P)) (dt-dec/dterr :bad-format))
    (loop [i 1, itm empty-itm, datepart? true, havefield? false]
      (if (>= i n)
        (itm->interval itm)
        (if (= (.charAt s i) \T)
          (recur (inc i) itm false false)
          (let [fieldstart i
                [val fval j] (parse-iso-number s i)
                unit (when (< j n) (.charAt s j))
                k (inc j)

                ;; The extended date tail: `yyyy`, then `-mm`, then
                ;; `-dd`, stopping at the end or at `T`. Shared by the
                ;; `-` arm and by the width-8 fallthrough.
                ext-date
                (fn []
                  (when havefield? (dt-dec/dterr :bad-format))
                  (let [itm (-> itm (adjust-years val 1) (adjust-fract-years fval 1))]
                    (cond
                      (nil? unit) [:return (itm->interval itm)]
                      (= unit \T) [:recur k itm false false]
                      :else
                      (let [[v2 f2 j2] (parse-iso-number s k)
                            itm (-> itm (adjust-months v2)
                                    (adjust-fract-days f2 days-per-month))
                            c2 (when (< j2 n) (.charAt s j2))]
                        (cond
                          (nil? c2) [:return (itm->interval itm)]
                          (= c2 \T) [:recur (inc j2) itm false false]
                          (not= c2 \-) (dt-dec/dterr :bad-format)
                          :else
                          (let [[v3 f3 j3] (parse-iso-number s (inc j2))
                                itm (-> itm (adjust-days v3 1)
                                        (adjust-fract-microseconds f3 usecs-per-day))
                                c3 (when (< j3 n) (.charAt s j3))]
                            (cond
                              (nil? c3) [:return (itm->interval itm)]
                              (= c3 \T) [:recur (inc j3) itm false false]
                              :else (dt-dec/dterr :bad-format))))))))

                ;; The extended time tail: `hh`, then `:mm`, then `:ss`.
                ext-time
                (fn []
                  (when havefield? (dt-dec/dterr :bad-format))
                  (let [itm (adjust-microseconds itm val fval usecs-per-hour)]
                    (if (nil? unit)
                      [:return (itm->interval itm)]
                      (let [[v2 f2 j2] (parse-iso-number s k)
                            itm (adjust-microseconds itm v2 f2 usecs-per-minute)
                            c2 (when (< j2 n) (.charAt s j2))]
                        (cond
                          (nil? c2) [:return (itm->interval itm)]
                          (not= c2 \:) (dt-dec/dterr :bad-format)
                          :else
                          (let [[v3 f3 j3] (parse-iso-number s (inc j2))
                                itm (adjust-microseconds itm v3 f3 usecs-per-sec)]
                            (if (>= j3 n)
                              [:return (itm->interval itm)]
                              (dt-dec/dterr :bad-format))))))))

                step
                (if datepart?
                  (case unit
                    \Y [:recur k (-> itm (adjust-years val 1)
                                     (adjust-fract-years fval 1)) true true]
                    \M [:recur k (-> itm (adjust-months val)
                                     (adjust-fract-days fval days-per-month)) true true]
                    \W [:recur k (-> itm (adjust-days val 7)
                                     (adjust-fract-days fval 7)) true true]
                    \D [:recur k (-> itm (adjust-days val 1)
                                     (adjust-fract-microseconds fval usecs-per-day))
                        true true]
                    ;; `T` and end-of-string try the width-8 BASIC form
                    ;; first and fall through to the extended one.
                    (\T nil)
                    (if (and (= 8 (iso-integer-width s fieldstart)) (not havefield?))
                      (let [itm (-> itm
                                    (adjust-years (quot val 10000) 1)
                                    (adjust-months (mod (quot val 100) 100))
                                    (adjust-days (mod val 100) 1)
                                    (adjust-fract-microseconds fval usecs-per-day))]
                        (if (nil? unit)
                          [:return (itm->interval itm)]
                          [:recur k itm false false]))
                      (ext-date))
                    \- (ext-date)
                    (dt-dec/dterr :bad-format))
                  (case unit
                    \H [:recur k (adjust-microseconds itm val fval usecs-per-hour)
                        false true]
                    \M [:recur k (adjust-microseconds itm val fval usecs-per-minute)
                        false true]
                    \S [:recur k (adjust-microseconds itm val fval usecs-per-sec)
                        false true]
                    nil
                    (if (and (= 6 (iso-integer-width s fieldstart)) (not havefield?))
                      [:return (itm->interval
                                (-> itm
                                    (adjust-microseconds (quot val 10000) 0.0
                                                         usecs-per-hour)
                                    (adjust-microseconds (mod (quot val 100) 100) 0.0
                                                         usecs-per-minute)
                                    (adjust-microseconds (mod val 100) 0.0
                                                         usecs-per-sec)
                                    (adjust-fract-microseconds fval 1)))]
                      (ext-time))
                    \: (ext-time)
                    (dt-dec/dterr :bad-format)))]
            (if (= (first step) :return)
              (second step)
              (let [[_ i' itm' dp' hf'] step]
                (recur i' itm' dp' hf')))))))))
