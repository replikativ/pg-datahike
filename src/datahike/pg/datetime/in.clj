(ns datahike.pg.datetime.in
  "The five input functions: `date_in`, `time_in`, `timetz_in`,
   `timestamp_in`, `timestamptz_in`.

   Each is the same three steps -- lex, decode, convert -- differing in
   the work-buffer size, which decoder it calls, whether it accepts a
   zone, and what it does with the parts it does not want. Those
   differences are small and every one of them is observable, so the
   five are written out rather than generated from a table.

   `date_in` is the one that surprises: it runs the WHOLE timestamp
   decoder and then discards the time. So the time is still validated
   and `'2001-02-03 25:00:00'::date` is an error, not 2001-02-03
   (date.c:117-170).

   ALL FIVE pass a non-NULL `tzp`, so all five ACCEPT a zone --
   `date_in` at date.c:134 and `time_in` at date.c:1399. The ones that
   have nowhere to put it parse it and discard it, which is why
   `'2000-01-01 12:00:00 PST'::date` is 2000-01-01 and `::time` is
   12:00:00 rather than errors. I had both refusing, on the assumption
   that a type with no zone would not take one; `tzp == NULL` is for
   other internal callers, not for the input functions.

   ERRORS. Every `dterr` becomes the SQLSTATE `DateTimeParseError`
   gives it (datetime.c:4063-4120):

     bad-format          22007  invalid input syntax for type …
     field-overflow      22008  date/time field value out of range
     md-field-overflow   22008  …plus a \"datestyle\" HINT
     tzdisp-overflow     22009  time zone displacement out of range
     bad-timezone        22023  time zone \"…\" not recognized

   The three overflow codes are distinct and are routinely got wrong:
   a zone past ±15 is 22009, not 22008."
  (:require [datahike.pg.datetime.decode :as dec]
            [datahike.pg.datetime.lex :as lex]
            [datahike.pg.datetime.parse :as parse]
            [datahike.pg.datetime.zone :as zone]
            [datahike.pg.errors :as errors]))

(def ^:private dterr->error
  {:bad-format [:invalid-datetime-format nil]
   :field-overflow [:datetime-field-overflow nil]
   :md-field-overflow [:datetime-field-overflow
                       "Perhaps you need a different \"datestyle\" setting."]
   :tzdisp-overflow [:invalid-tz-displacement nil]
   :bad-timezone [:invalid-parameter-value nil]})

(defn- raise!
  "A `dterr` as the SQL error for `type-name` and the ORIGINAL literal.
   The decoder never sees either, which is why the mapping lives here."
  [kind ^String type-name ^String value]
  (let [[cat hint] (get dterr->error kind [:invalid-datetime-format nil])]
    (throw (errors/pg-error
            cat
            (cond-> {:type type-name :value value}
              (= cat :invalid-parameter-value)
              (assoc :message (str "time zone \"" value "\" not recognized"))
              hint (assoc :hint hint))))))

(defmacro ^:private with-dterr
  "Run `body`, turning any `dterr` into the SQL error."
  [type-name value & body]
  `(try ~@body
        (catch clojure.lang.ExceptionInfo e#
          (if-let [k# (::lex/dterr (ex-data e#))]
            (raise! k# ~type-name ~value)
            (throw e#)))))

(defn- resolve-offset
  "A recorded zone to SECONDS WEST, now that the date is known."
  ^long [zone tm session-zone]
  (case (:kind zone)
    :offset (:west zone)
    :named (if-let [z (zone/resolve-zone-name (:name zone))]
             (zone/determine-offset tm z)
             (dec/dterr :bad-timezone))
    :abbrev (zone/determine-abbrev-offset tm (:zone zone))
    :session (zone/determine-offset tm session-zone)
    0))

;; ---------------------------------------------------------------------------
;; The canonical forms
;;
;; Each input function returns PostgreSQL's OWN internal
;; representation, not the decoded fields:
;;
;;   date         `:jd`,   days from 2000-01-01
;;   time/timetz  `:usec`, microseconds since midnight (plus `:west`)
;;   timestamp    `:usec`, microseconds from 2000-01-01 00:00:00
;;
;; That is not a stylistic choice -- it is where the NORMALIZATION
;; lives. `tm2timestamp` is `date * USECS_PER_DAY + time2t(h,m,s,fsec)`
;; with no special cases, so hour 24 and second 60 simply carry into
;; the total. Returning the raw fields instead would leave
;; `'2001-02-03 24:00:00'` printing as written, where PostgreSQL gives
;; `2001-02-04 00:00:00`, and `'23:59:60'::time` as written where
;; PostgreSQL gives `24:00:00`.
;;
;; TIME is the exception that proves it: there is no day to carry
;; into, so 86400000000 is a legal value and renders as `24:00:00`.
;;
;; A carrier type is deliberately not used. `java.util.Date` cannot
;; hold what these produce -- microseconds, year 0 and below, the two
;; infinities -- and handing back the canonical integer keeps the lossy
;; step visible at the call site instead of buried here.
;; ---------------------------------------------------------------------------

(def ^:const pg-epoch-jdate
  "POSTGRES_EPOCH_JDATE: the Julian day of 2000-01-01."
  2451545)

(def ^:const usecs-per-day 86400000000)

(defn- time->usec ^long [{:keys [hour min sec usec]}]
  (+ (* (long hour) 3600000000) (* (long min) 60000000)
     (* (long sec) 1000000) (long usec)))

(defn- valid-julian?
  "IS_VALID_JULIAN (timestamp.h:219): 4714 BC to 5874898 AD."
  [{:keys [year mon mday]}]
  (let [jd (dec/date2j year mon mday)]
    (and (>= jd 0) (< jd 2147483647))))

(def ^:const min-timestamp
  "MIN_TIMESTAMP (timestamp.h): 4714-11-24 BC."
  -211813488000000000)

(def ^:const end-timestamp
  "END_TIMESTAMP (timestamp.h): just past 294277-01-09 AD."
  9223371331200000000)

(defn- tm->timestamp
  "`tm2timestamp`. Fields to microseconds from 2000-01-01, with the
   zone applied when there is one. Out of range is 22008 -- the error
   says `timestamp out of range`, not `field value out of range`."
  ^long [tm west ^String type-name ^String value]
  (when-not (valid-julian? tm) (raise! :field-overflow type-name value))
  (let [date (- (dec/date2j (:year tm) (:mon tm) (:mday tm)) pg-epoch-jdate)
        t (time->usec tm)
        ;; The C detects this as integer overflow and returns -1, which
        ;; the caller reports as "timestamp out of range". A long
        ;; overflows silently in Clojure, so the day count is bounded
        ;; BEFORE the multiply rather than after -- 5874897-12-31 threw
        ;; ArithmeticException here until it was.
        _ (when (> (Math/abs (long date)) 110000000)
            (throw (errors/pg-error :datetime-field-overflow
                                    {:message "timestamp out of range"})))
        r (+ (* date usecs-per-day) t)
        r (if west (+ r (* (long west) 1000000)) r)]
    (when (or (< r min-timestamp) (>= r end-timestamp))
      (throw (errors/pg-error :datetime-field-overflow
                              {:message "timestamp out of range"})))
    r))

(defn date-in
  "`date_in` (date.c:117-170). Returns `{:kind :date :jd …}` with `:jd`
   in days from 2000-01-01, or `{:kind :infinity|:-infinity}`.

   It decodes a whole TIMESTAMP and then throws the time away, so the
   time is validated first: `'2001-02-03 25:00:00'::date` is an error.
   And it passes no `tzp`, so any zone at all is refused."
  [^String s {:keys [date-order now] :as _ctx}]
  (with-dterr "date" s
    (let [r (parse/decode-datetime (lex/tokenize s (:date lex/buflen-for))
                                   {:date-order date-order :now now :zone? true})]
      (case (:dtype r)
        :date (let [tm (:tm r)]
                (when-not (valid-julian? tm) (raise! :field-overflow "date" s))
                {:kind :date :jd (- (dec/date2j (:year tm) (:mon tm) (:mday tm))
                                    pg-epoch-jdate)})
        :late {:kind :infinity}
        :early {:kind :-infinity}
        :epoch {:kind :date :jd (- 2440588 pg-epoch-jdate)}))))

(defn time-in
  "`time_in` (date.c:1368-1404). Microseconds since midnight.
   86400000000 is legal and renders as `24:00:00` -- there is no day to
   carry into. No zone."
  [^String s {:keys [date-order now] :as _ctx}]
  (with-dterr "time" s
    (let [r (parse/decode-time-only (lex/tokenize s (:time lex/buflen-for))
                                    {:date-order date-order :now now :zone? true})]
      {:kind :time :usec (time->usec (:tm r))})))

(defn timetz-in
  "`timetz_in` (date.c:2266-2302). As `time-in`, plus the offset.

   A zone whose offset depends on the date can only be used when the
   literal supplies the date, or when the zone is a FIXED offset --
   which is why `'12:00 UTC'::timetz` works and
   `'12:00 America/New_York'::timetz` does not."
  [^String s {:keys [date-order now session-zone] :as _ctx}]
  (with-dterr "time with time zone" s
    (let [r (parse/decode-time-only (lex/tokenize s (:timetz lex/buflen-for))
                                    {:date-order date-order :now now :zone? true})
          z (:zone r)
          at (merge (:resolve-date r) (:tm r))
          west (if (= (:kind z) :named)
                 (if-let [zi (zone/resolve-zone-name (:name z))]
                   (or (zone/fixed-offset zi) (zone/determine-offset at zi))
                   (dec/dterr :bad-timezone))
                 (resolve-offset z at session-zone))]
      {:kind :timetz :usec (time->usec (:tm r)) :west west})))

(defn timestamp-in
  "`timestamp_in` (timestamp.c:160-220). No zone is kept -- the literal
   may CARRY one and it is parsed and then ignored, which is why
   `'2001-02-03 04:05:06+05:30:30'::timestamp` is 04:05:06 unshifted."
  [^String s {:keys [date-order now] :as _ctx}]
  (with-dterr "timestamp" s
    (let [r (parse/decode-datetime (lex/tokenize s (:timestamp lex/buflen-for))
                                   {:date-order date-order :now now :zone? true})]
      (case (:dtype r)
        :date {:kind :timestamp :usec (tm->timestamp (:tm r) nil "timestamp" s)}
        :late {:kind :infinity}
        :early {:kind :-infinity}
        ;; `epoch` is 1970-01-01, which is BEFORE 2000-01-01, so the
        ;; day count is already negative -- negating it again put this
        ;; in 2029.
        :epoch {:kind :timestamp :usec (* (- 2440588 pg-epoch-jdate)
                                          usecs-per-day)}))))

(defn timestamptz-in
  "`timestamptz_in` (timestamp.c:412-470). As `timestamp-in`, and the
   zone IS kept -- resolved here, after the date is known, and folded
   into the value so that what comes back is a UTC instant."
  [^String s {:keys [date-order now session-zone] :as _ctx}]
  (with-dterr "timestamp with time zone" s
    (let [r (parse/decode-datetime (lex/tokenize s (:timestamptz lex/buflen-for))
                                   {:date-order date-order :now now :zone? true})]
      (case (:dtype r)
        :date {:kind :timestamptz
               :usec (tm->timestamp (:tm r)
                                    (resolve-offset (:zone r) (:tm r) session-zone)
                                    "timestamp with time zone" s)}
        :late {:kind :infinity}
        :early {:kind :-infinity}
        :epoch {:kind :timestamptz :usec (* (- 2440588 pg-epoch-jdate)
                                            usecs-per-day)}))))

(defn ->fields
  "A canonical value back to `{:year :mon :mday :hour :min :sec :usec}`,
   which is `j2date` plus the division `timestamp2tm` does. For a
   `:timetz` the fields are the LOCAL time; `:west` carries the rest."
  [{:keys [kind jd usec]}]
  (case kind
    :date (let [[y m d] (dec/j2date (+ jd pg-epoch-jdate))]
            {:year y :mon m :mday d})
    (:time :timetz)
    {:hour (quot usec 3600000000) :min (quot (mod usec 3600000000) 60000000)
     :sec (quot (mod usec 60000000) 1000000) :usec (mod usec 1000000)}
    (:timestamp :timestamptz)
    (let [days (long (Math/floorDiv (long usec) (long usecs-per-day)))
          rem (long (Math/floorMod (long usec) (long usecs-per-day)))
          [y m d] (dec/j2date (+ days pg-epoch-jdate))]
      {:year y :mon m :mday d
       :hour (quot rem 3600000000) :min (quot (mod rem 3600000000) 60000000)
       :sec (quot (mod rem 60000000) 1000000) :usec (mod rem 1000000)})
    nil))
