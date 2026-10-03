(ns datahike.pg.datetime.decode
  "The leaf decoders of PostgreSQL's datetime parser: the small functions
   `DecodeDateTime` and `DecodeTimeOnly` call to turn ONE lexed field
   into numbers. They are separated from the state machine because each
   is independently testable and several have rules no one would guess.

   SIGN CONVENTION. PostgreSQL carries a zone offset internally as
   SECONDS WEST of Greenwich -- `DecodeTimezone` computes seconds east
   and then stores `*tzp = -tz` (datetime.c:3060). The abbreviation
   table, and `tznames/Default` that it comes from, are SECONDS EAST.
   Everything here is named for which one it is, because a sign error
   between them is a silent double offset rather than a failure.

   ERRORS are `ex-info` carrying `::dterr`, mirroring the C's DTERR_*
   returns. The entry points map those to SQLSTATEs; nothing here knows
   about SQL."
  (:require [datahike.pg.datetime.lex :as lex]))

(defn dterr
  "A DTERR_* return, as a throw. The C returns these as ints and every
   caller checks; a throw is the same control flow with less to forget."
  [kind]
  (throw (ex-info (name kind) {::lex/dterr kind})))

;; ---------------------------------------------------------------------------
;; Julian day conversions (datetime.c:286-333)
;; ---------------------------------------------------------------------------

(defn date2j
  "`date2j` (datetime.c:286-308): Gregorian date to Julian day number.

   Transcribed with its integer arithmetic intact. The `7834 * month /
   256` is a trick for the 30.6-day month step and only works with
   truncating integer division -- `quot`, not `/`. The function is valid
   for any year including negative ones, which is how BC dates work."
  ^long [^long year ^long month ^long day]
  (let [m2 (> month 2)
        month (if m2 (+ month 1) (+ month 13))
        year (if m2 (+ year 4800) (+ year 4799))
        century (quot year 100)]
    (+ (- (* year 365) 32167)
       (- (quot year 4) century)
       (quot century 4)
       (quot (* 7834 month) 256)
       day)))

(defn j2date
  "`j2date` (datetime.c:311-333): Julian day number back to
   `[year month day]`.

   The C declares `julian`, `quad` and `extra` as UNSIGNED int and
   relies on 32-bit wraparound nowhere in the valid range -- but the
   intermediate `(julian - quad * 146097) * 4 + 3` would overflow a
   signed 32-bit int for large inputs, which is why they are unsigned.
   Clojure longs are 64-bit, so the arithmetic is exact and no masking
   is needed; the division and `%` stay truncating to match."
  [^long jd]
  (let [julian (+ jd 32044)
        quad (quot julian 146097)
        extra (+ (* (- julian (* quad 146097)) 4) 3)
        julian (+ julian 60 (* quad 3) (quot extra 146097))
        quad2 (quot julian 1461)
        julian (- julian (* quad2 1461))
        y (quot (* julian 4) 1461)
        julian (+ (if (not= y 0) (mod (+ julian 305) 365) (mod (+ julian 306) 366))
                  123)
        y (+ y (* quad2 4))
        quad3 (quot (* julian 2141) 65536)]
    [(- y 4800)
     (inc (mod (+ quad3 10) 12))
     (- julian (quot (* 7834 quad3) 256))]))

(defn j2day
  "`j2day` (datetime.c:338-352): Julian day to day-of-week, 0..6 =
   Sun..Sat."
  ^long [^long jd]
  (mod (+ jd 1) 7))

(def ^:private day-tab
  "`day_tab` (datetime.c:75-79). Two rows, common year and leap."
  [[31 28 31 30 31 30 31 31 30 31 30 31]
   [31 29 31 30 31 30 31 31 30 31 30 31]])

(defn leap-year?
  "`isleap` (datetime.h:271). Applied to the PROLEPTIC year as
   PostgreSQL stores it, where 1 BC is year 0 -- so year 0 is a leap
   year and `0000-02-29 BC` is a real date."
  [^long y]
  (and (zero? (mod y 4))
       (or (not (zero? (mod y 100))) (zero? (mod y 400)))))

(defn days-in-month ^long [^long y ^long m]
  (nth (nth day-tab (if (leap-year? y) 1 0)) (dec m)))

;; ---------------------------------------------------------------------------
;; ValidateDate (datetime.c:2508-2576)
;; ---------------------------------------------------------------------------

(defn validate-date
  "`ValidateDate`. Applies the year rules, resolves a day-of-year, and
   range-checks -- in that order, because each step needs the previous
   one's answer.

   `fields` is the set of date fields that were actually SET, which is
   the `fmask` test in the C; a check that a field is in range must not
   run on a field nobody supplied.

   Returns the corrected `tm`, or throws.

   Two rules here are load-bearing and neither is obvious:

   There is no year zero in AD/BC notation, so a non-Julian year <= 0 is
   an overflow -- but `isjulian` SKIPS the check entirely, because a
   Julian day legitimately produces year 0 and below.

   The 2-digit year rule (<70 => +2000, <100 => +1900) is applied only
   when `is2digits` AND NOT `bc`: the branches are an if/else chain with
   `bc` first. So `'70-01-01'` is 1970 and `'70-01-01 BC'` is 70 BC, not
   1970 BC."
  [{:keys [year mon mday yday] :as tm} fields
   {:keys [julian? two-digits? bc?]}]
  (let [tm (if (contains? fields :year)
             (cond
               julian? tm
               bc? (if (<= year 0)
                     (dterr :field-overflow)
                     ;; 1 BC is stored as year 0, 2 BC as -1.
                     (assoc tm :year (- (dec year))))
               two-digits? (cond
                             (neg? year) (dterr :field-overflow)
                             (< year 70) (assoc tm :year (+ year 2000))
                             (< year 100) (assoc tm :year (+ year 1900))
                             :else tm)
               :else (if (<= year 0) (dterr :field-overflow) tm))
             tm)
        ;; The day-of-year is resolved only NOW, because it needs the
        ;; corrected year to know whether the year is a leap year.
        tm (if (contains? fields :doy)
             (let [[y m d] (j2date (+ (date2j (:year tm) 1 1) yday -1))]
               (assoc tm :year y :mon m :mday d))
             tm)]
    (when (and (contains? fields :month)
               (or (< (:mon tm) 1) (> (:mon tm) 12)))
      (dterr :md-field-overflow))
    (when (and (contains? fields :day)
               (or (< (:mday tm) 1) (> (:mday tm) 31)))
      (dterr :md-field-overflow))
    ;; Only with year, month AND day all known can the real month length
    ;; be checked. The C notes that this one is FIELD_OVERFLOW rather
    ;; than MD_FIELD_OVERFLOW, because "Feb 29" is unlikely to be a
    ;; field-order mistake and so should not suggest a DateStyle fix.
    (when (and (every? fields [:year :month :day])
               (> (:mday tm) (days-in-month (:year tm) (:mon tm))))
      (dterr :field-overflow))
    tm))

;; ---------------------------------------------------------------------------
;; DecodeTimezone (datetime.c:3007-3067)
;; ---------------------------------------------------------------------------

(defn- strtoint
  "`strtoint` over `s` from `i`: an optional sign, then digits. Returns
   `[value end-index]`, with `end-index` = `i` when nothing was
   consumed. Out of int range is `:tzdisp-overflow` in every caller
   here, so it throws that directly."
  [^String s ^long i]
  (let [n (.length s)
        neg? (and (< i n) (= (.charAt s i) \-))
        plus? (and (< i n) (= (.charAt s i) \+))
        start (if (or neg? plus?) (inc i) i)
        end (loop [k start]
              (if (and (< k n) (Character/isDigit (.charAt s k))) (recur (inc k)) k))]
    (if (= end start)
      [0 i]
      (let [v (try (Long/parseLong (subs s start end))
                   (catch NumberFormatException _ (dterr :tzdisp-overflow)))]
        (when (or (> v Integer/MAX_VALUE) (< v Integer/MIN_VALUE))
          (dterr :tzdisp-overflow))
        [(if neg? (- v) v) end]))))

(def ^:const max-tzdisp-hour
  "MAX_TZDISP_HOUR (timestamp.h). The largest zone displacement
   PostgreSQL accepts -- not 24, and not 14 either."
  15)

(defn decode-timezone
  "`DecodeTimezone`. A numeric zone field (`+05`, `-08:30`, `+0530`,
   `+05:30:30`) to SECONDS WEST of Greenwich.

   The bare-digits rule is the part that surprises: with no colon, a
   string LONGER THAN 3 characters is split as hhmm, and a shorter one
   is hours. So `+05` is five hours, `+0530` is five and a half -- and
   `+100` is ONE hour, because it is four characters and splits to
   hr=1, min=0. Length, not value.

   The trailing check (`*cp != '\\0'`) happens AFTER the arithmetic but
   still rejects: `+05.5` is a format error, not five and a half hours.
   The lexer's greedy run over `. : -` exists precisely so that the
   whole of `+05.5` arrives here to be refused, rather than `.5`
   escaping into the fraction."
  ^long [^String s]
  (when-not (and (pos? (.length s))
                 (or (= (.charAt s 0) \+) (= (.charAt s 0) \-)))
    (dterr :bad-format))
  (let [n (.length s)
        [hr cp] (strtoint s 1)
        [hr min sec cp]
        (cond
          (and (< cp n) (= (.charAt s cp) \:))
          (let [[mn cp] (strtoint s (inc cp))]
            (if (and (< cp n) (= (.charAt s cp) \:))
              (let [[sc cp] (strtoint s (inc cp))] [hr mn sc cp])
              [hr mn 0 cp]))

          ;; No colon and nothing left: hhmm if the whole string is
          ;; longer than three characters.
          (and (= cp n) (> n 3)) [(quot hr 100) (mod hr 100) 0 cp]

          :else [hr 0 0 cp])]
    (when (or (neg? hr) (> hr max-tzdisp-hour)) (dterr :tzdisp-overflow))
    (when (or (neg? min) (>= min 60)) (dterr :tzdisp-overflow))
    (when (or (neg? sec) (>= sec 60)) (dterr :tzdisp-overflow))
    (let [tz (+ (* (+ (* hr 60) min) 60) sec)
          tz (if (= (.charAt s 0) \-) (- tz) tz)]
      ;; The C computes tz and assigns *tzp BEFORE this check, then
      ;; returns the error anyway -- the assignment is dead. Checked
      ;; after, here, for the same observable behaviour.
      (when (not= cp n) (dterr :bad-format))
      ;; *tzp = -tz: seconds WEST.
      (- tz))))

;; ---------------------------------------------------------------------------
;; DecodeTimeCommon (datetime.c:2583-2659)
;; ---------------------------------------------------------------------------

(defn- parse-fraction
  "`ParseFraction` (datetime.c:2415-2440): `.` then digits, as a double.
   A lone `.` is zero, which some strtod versions would reject. Anything
   left over is a format error."
  ^double [^String s ^long i]
  (if (= (inc i) (.length s))
    0.0
    (let [sub (subs s i)]
      (try (let [d (Double/parseDouble sub)]
             (when (Double/isNaN d) (dterr :bad-format))
             d)
           (catch NumberFormatException _ (dterr :bad-format))))))

(defn decode-time
  "`DecodeTimeCommon`/`DecodeTime` (datetime.c:2583-2683). A `:time`
   field to `{:hour :min :sec :usec}`.

   `hh:mm.ff` is NOT hours, minutes and a fraction of a minute: the C
   shifts the fields down (`tm_sec = tm_min; tm_min = tm_hour;
   tm_hour = 0`, datetime.c:2622-2628), so `04:05.5` is four MINUTES and
   five and a half seconds. That is for `DecodeInterval`'s benefit and
   it applies here all the same.

   Seconds may be exactly 60 (`> SECS_PER_MINUTE` is the failure, not
   `>=`): `23:59:60` is a legal input, normalized afterwards. Minutes
   may not -- `MINS_PER_HOUR - 1` is the limit there.

   The hour has NO upper bound in this function. `25:00:00` and
   `24:00:00` both decode fine and are rejected, or not, by the caller:
   `DecodeTimeOnly` allows exactly 24:00:00 and `tm2time` wraps nothing.
   Putting the hour limit here would reject `24:00:00`, which is legal."
  [^String s]
  (let [n (.length s)
        [hour cp] (strtoint s 0)]
    (when-not (and (< cp n) (= (.charAt s cp) \:)) (dterr :bad-format))
    (let [[minute cp] (strtoint s (inc cp))
          c (when (< cp n) (.charAt s cp))
          [hour minute sec fsec]
          (cond
            (nil? c) [hour minute 0 0.0]

            (= c \.) (let [frac (parse-fraction s cp)]
                       ;; The shift: hh:mm.ff means mm:ss.ff.
                       [0 hour minute frac])

            (= c \:) (let [[sc cp2] (strtoint s (inc cp))
                           c2 (when (< cp2 n) (.charAt s cp2))]
                       (cond
                         (= c2 \.) [hour minute sc (parse-fraction s cp2)]
                         (nil? c2) [hour minute sc 0.0]
                         :else (dterr :bad-format)))

            :else (dterr :bad-format))]
      (when (or (neg? hour)
                (neg? minute) (> minute 59)
                (neg? sec) (> sec 60)
                (neg? fsec) (> fsec 1.0))
        (dterr :field-overflow))
      {:hour hour :min minute :sec sec
       :usec (Math/round (* fsec 1000000.0))})))
