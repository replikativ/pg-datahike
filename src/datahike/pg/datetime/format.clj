(ns datahike.pg.datetime.format
  "`to_char` over timestamps, timestamptz, dates and intervals -- a port of
   formatting.c's DCH side (parse_format, DCH_keywords / DCH_suff,
   DCH_to_char), PostgreSQL 17.

   Ported rather than mapped onto java.time patterns, because the two
   languages only look alike: PostgreSQL matches a keyword by the FIRST
   table entry that prefixes the input (`HH12` before `HH`, `DDD` before
   `DD`), lets `FM`/`TM` prefix and `TH`/`th`/`SP` follow any keyword,
   pads a month or day NAME to nine characters, counts centuries so that
   20 is 1901-2000, numbers BC years from 1 (astronomical year 0 is 1 BC),
   and prints a negative interval field with an extra digit of width.

   The input side (to_date / to_timestamp, DCH_from_char) is not here."
  (:require [clojure.string :as str]
            [datahike.pg.errors :as errors]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Keywords and suffixes, in DCH_keywords' order

(def ^:private keywords
  "[name id] in DCH_keywords' order. Several names share an id -- the
   lower-case numeric spellings, and SSSSS with SSSS -- exactly as there."
  [["A.D." :A_D] ["A.M." :A_M] ["AD" :AD] ["AM" :AM]
   ["B.C." :B_C] ["BC" :BC] ["CC" :CC]
   ["DAY" :DAY] ["DDD" :DDD] ["DD" :DD] ["DY" :DY] ["Day" :Day] ["Dy" :Dy] ["D" :D]
   ["FF1" :FF1] ["FF2" :FF2] ["FF3" :FF3] ["FF4" :FF4] ["FF5" :FF5] ["FF6" :FF6] ["FX" :FX]
   ["HH24" :HH24] ["HH12" :HH12] ["HH" :HH]
   ["IDDD" :IDDD] ["ID" :ID] ["IW" :IW] ["IYYY" :IYYY] ["IYY" :IYY] ["IY" :IY] ["I" :I]
   ["J" :J] ["MI" :MI] ["MM" :MM] ["MONTH" :MONTH] ["MON" :MON] ["MS" :MS]
   ["Month" :Month] ["Mon" :Mon] ["OF" :OF] ["P.M." :P_M] ["PM" :PM] ["Q" :Q] ["RM" :RM]
   ["SSSSS" :SSSS] ["SSSS" :SSSS] ["SS" :SS] ["TZH" :TZH] ["TZM" :TZM] ["TZ" :TZ] ["US" :US]
   ["WW" :WW] ["W" :W] ["Y,YYY" :Y_YYY] ["YYYY" :YYYY] ["YYY" :YYY] ["YY" :YY] ["Y" :Y]
   ["a.d." :a_d] ["a.m." :a_m] ["ad" :ad] ["am" :am] ["b.c." :b_c] ["bc" :bc] ["cc" :CC]
   ["day" :day] ["ddd" :DDD] ["dd" :DD] ["dy" :dy] ["d" :D]
   ["ff1" :FF1] ["ff2" :FF2] ["ff3" :FF3] ["ff4" :FF4] ["ff5" :FF5] ["ff6" :FF6] ["fx" :FX]
   ["hh24" :HH24] ["hh12" :HH12] ["hh" :HH]
   ["iddd" :IDDD] ["id" :ID] ["iw" :IW] ["iyyy" :IYYY] ["iyy" :IYY] ["iy" :IY] ["i" :I]
   ["j" :J] ["mi" :MI] ["mm" :MM] ["month" :month] ["mon" :mon] ["ms" :MS]
   ["of" :OF] ["p.m." :p_m] ["pm" :pm] ["q" :Q] ["rm" :rm]
   ["sssss" :SSSS] ["ssss" :SSSS] ["ss" :SS] ["tzh" :TZH] ["tzm" :TZM] ["tz" :tz] ["us" :US]
   ["ww" :WW] ["w" :W] ["y,yyy" :Y_YYY] ["yyyy" :YYYY] ["yyy" :YYY] ["yy" :YY] ["y" :Y]])

(def ^:private keywords-by-char
  (group-by #(.charAt ^String (first %) 0) keywords))

(def ^:private prefix-suffixes [["FM" :FM] ["fm" :FM] ["TM" :TM] ["tm" :TM]])
(def ^:private postfix-suffixes [["TH" :TH] ["th" :th] ["SP" :SP]])

(defn- starts-with-at? [^String s ^long i ^String prefix]
  (.startsWith s prefix (int i)))

(defn- separator-char? [c]
  (let [c (int c)]
    (and (> c 0x20) (< c 0x7F)
         (not (<= (int \A) c (int \Z)))
         (not (<= (int \a) c (int \z)))
         (not (<= (int \0) c (int \9))))))

(defn parse-format
  "parse_format for the DCH flag: a vector of nodes, either
   {:key id :suffix #{…}} for a keyword or {:text s} for literal output."
  [^String fmt]
  (let [n (count fmt)]
    (loop [i 0 out []]
      (if (>= i n)
        out
        (let [[pre-id pre-len] (some (fn [[name id]] (when (starts-with-at? fmt i name) [id (count name)]))
                                     prefix-suffixes)
              j (+ i (or pre-len 0))
              kw (when (< j n)
                   (some (fn [[name id]] (when (starts-with-at? fmt j name) [name id]))
                         (get keywords-by-char (.charAt fmt j))))]
          (cond
            kw
            (let [k (+ j (count (first kw)))
                  [post-id post-len] (when (< k n)
                                       (some (fn [[name id]] (when (starts-with-at? fmt k name) [id (count name)]))
                                             postfix-suffixes))]
              (recur (long (+ k (long (or post-len 0))))
                     (conj out {:key (second kw)
                                :name (first kw)
                                :suffix (cond-> #{} pre-id (conj pre-id) post-id (conj post-id))})))

            ;; A prefix with nothing after it to modify: PostgreSQL has
            ;; already consumed it, and moves on.
            (>= j n) out

            (= \" (.charAt fmt j))
            ;; A double-quoted literal; a backslash quotes the next char.
            (let [[k text] (loop [k (inc j) sb (StringBuilder.)]
                             (cond
                               (>= k n) [k (str sb)]
                               (= \" (.charAt fmt k)) [(inc k) (str sb)]
                               (and (= \\ (.charAt fmt k)) (< (inc k) n))
                               (recur (+ k 2) (.append sb (.charAt fmt (inc k))))
                               :else (recur (inc k) (.append sb (.charAt fmt k)))))]
              (recur (long k) (conj out {:text text})))

            :else
            ;; Outside quotes a backslash is special only before a quote.
            (let [j (if (and (= \\ (.charAt fmt j)) (< (inc j) n) (= \" (.charAt fmt (inc j))))
                      (inc j) j)]
              (recur (long (inc (long j))) (conj out {:text (str (.charAt fmt (int j)))})))))))))

;; ---------------------------------------------------------------------------
;; Rendering

(def ^:private months ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])
(def ^:private months-full ["January" "February" "March" "April" "May" "June" "July"
                            "August" "September" "October" "November" "December"])
(def ^:private days ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])
(def ^:private days-short ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"])
(def ^:private rm-upper ["XII" "XI" "X" "IX" "VIII" "VII" "VI" "V" "IV" "III" "II" "I"])
(def ^:private rm-lower ["xii" "xi" "x" "ix" "viii" "vii" "vi" "v" "iv" "iii" "ii" "i"])

(defn- ordinal-suffix
  "get_th: ST/ND/RD/TH, every teen taking TH."
  [^String num upper?]
  (let [len (count num)
        last-c (.charAt num (dec len))
        last-c (if (and (> len 1) (= \1 (.charAt num (- len 2)))) \0 last-c)
        [up lo] (case last-c \1 ["ST" "st"] \2 ["ND" "nd"] \3 ["RD" "rd"] ["TH" "th"])]
    (when-not (Character/isDigit (.charAt num (dec len)))
      (throw (errors/pg-error :invalid-text-representation
                              {:message (str "\"" num "\" is not a number")})))
    (if upper? up lo)))

(defn- zero-pad
  "C's `%0*d`: width w, zero-padded after the sign; 0 means no padding."
  [^long v ^long w]
  (let [s (str (Math/abs v))
        neg (neg? v)
        pad (- w (count s) (if neg 1 0))]
    (str (when neg "-") (when (pos? pad) (apply str (repeat pad \0))) s)))

(defn- pad-right [^String s ^long w]
  (if (< (count s) w) (str s (apply str (repeat (- w (count s)) \space))) s))

(defn- date2j
  "PostgreSQL's date2j, the Julian day number (astronomical year)."
  ^long [^long y ^long m ^long d]
  (let [[y m] (if (> m 2) [(+ y 4800) (inc m)] [(+ y 4799) (+ m 13)])
        century (quot y 100)]
    (+ (- (* y 365) 32167)
       (quot y 4) (- century) (quot century 4)
       (quot (* 7834 m) 256) d)))

(defn- iso-fields
  "[iso-year iso-week iso-day-of-year] of a date, as date2isoyear,
   date2isoweek and date2isoyearday compute them."
  [^long y ^long m ^long d]
  (let [ld (java.time.LocalDate/of (int y) (int m) (int d))
        iy (.get ld java.time.temporal.IsoFields/WEEK_BASED_YEAR)
        iw (.get ld java.time.temporal.IsoFields/WEEK_OF_WEEK_BASED_YEAR)
        ;; days since the Monday of week 1
        week1 (-> (java.time.LocalDate/of (int iy) 1 4)
                  (.with java.time.temporal.ChronoField/DAY_OF_WEEK 1))
        iyd (inc (.between java.time.temporal.ChronoUnit/DAYS week1 ld))]
    [iy iw iyd]))

(defn- adj-year
  "ADJUST_YEAR: a BC year counts from 1, except in an interval."
  [y interval?]
  (let [y (long y)] (if interval? y (if (<= y 0) (- (dec y)) y))))

(defn- no-interval! [c]
  (when (:interval? c)
    (throw (errors/pg-error :invalid-datetime-format
                            {:message "invalid format specification for an interval value"
                             :hint "Intervals are not tied to specific calendar dates."}))))

(defn- num-out
  "A numeric field, with its TH/th suffix."
  [c ^String s]
  (if-let [th (:th c)] (str s (ordinal-suffix s (= th :upper))) s))

(defn- width
  "The `%0*d` width: none under FM, one more for a negative value."
  [c normal v]
  (if (:fm? c) 0 (if (>= (long v) 0) normal (inc (long normal)))))

(defn- name-out
  "A month or day name: padded to nine unless FM or TM."
  [c ^String s mode]
  (let [s (case mode :upper (str/upper-case s) :lower (str/lower-case s) s)]
    (if (or (:fm? c) (:tm? c)) s (pad-right s 9))))

(defn- ampm [c am pm] (if (>= (mod (long (:hour c)) 24) 12) pm am))

(defn- iso [c] (iso-fields (:year c) (:mon c) (:mday c)))

(defn- year-of [c key iso-key]
  (adj-year (if (= key iso-key) (nth (iso c) 0) (:year c)) (:interval? c)))

(def ^:private renderers
  "DCH_to_char's switch, one small function per keyword id: as a single
   `case` it exceeds the JVM's method size limit."
  (let [h12 (fn [c] (let [h (mod (long (:hour c)) 12)] (num-out c (zero-pad (if (zero? h) 12 h) (width c 2 (:hour c))))))
        fsec (fn [div w] (fn [c] (num-out c (zero-pad (quot (long (:usec c)) div) w))))
        month (fn [names mode] (fn [c] (no-interval! c)
                                 (when (pos? (long (:mon c)))
                                   (name-out c (names (dec (long (:mon c)))) mode))))
        mon (fn [f] (fn [c] (no-interval! c) (when (pos? (long (:mon c))) (f (months (dec (long (:mon c))))))))
        day (fn [mode] (fn [c] (no-interval! c) (name-out c (days (:wday c)) mode)))
        dy (fn [f] (fn [c] (no-interval! c) (f (days-short (:wday c)))))
        era (fn [bc ad] (fn [c] (no-interval! c) (if (<= (long (:year c)) 0) bc ad)))
        yyyy (fn [mod-by normal iso-key]
               (fn [c] (let [y (year-of c (:key c) iso-key)
                             v (if mod-by (rem y mod-by) y)]
                         (num-out c (zero-pad v (if (:fm? c) 0 (if (>= y 0) normal (inc normal))))))))
        rm (fn [names]
             (fn [c] (let [mon (long (:mon c)) year (long (:year c))]
                       (when-not (and (zero? mon) (zero? year))
                         (let [m (cond (zero? mon) (if (>= year 0) 0 11)
                                       (neg? mon) (- (inc mon))
                                       :else (- 12 mon))
                               s (names m)]
                           (if (:fm? c) s (pad-right s 4)))))))]
    {:A_M (fn [c] (ampm c "A.M." "P.M.")) :P_M (fn [c] (ampm c "A.M." "P.M."))
     :AM (fn [c] (ampm c "AM" "PM")) :PM (fn [c] (ampm c "AM" "PM"))
     :a_m (fn [c] (ampm c "a.m." "p.m.")) :p_m (fn [c] (ampm c "a.m." "p.m."))
     :am (fn [c] (ampm c "am" "pm")) :pm (fn [c] (ampm c "am" "pm"))
     :HH h12 :HH12 h12
     :HH24 (fn [c] (num-out c (zero-pad (:hour c) (width c 2 (:hour c)))))
     :MI (fn [c] (num-out c (zero-pad (:min c) (width c 2 (:min c)))))
     :SS (fn [c] (num-out c (zero-pad (:sec c) (width c 2 (:sec c)))))
     :FF1 (fsec 100000 1) :FF2 (fsec 10000 2) :FF3 (fsec 1000 3) :MS (fsec 1000 3)
     :FF4 (fsec 100 4) :FF5 (fsec 10 5) :FF6 (fsec 1 6) :US (fsec 1 6)
     :SSSS (fn [c] (num-out c (str (+ (* (long (:hour c)) 3600) (* (long (:min c)) 60) (long (:sec c))))))
     :tz (fn [c] (no-interval! c) (some-> (:tzn c) str/lower-case))
     :TZ (fn [c] (no-interval! c) (:tzn c))
     :TZH (fn [c] (no-interval! c)
            (let [g (long (or (:gmtoff c) 0))]
              (str (if (>= g 0) "+" "-") (zero-pad (quot (Math/abs g) 3600) 2))))
     :TZM (fn [c] (no-interval! c)
            (zero-pad (quot (rem (Math/abs (long (or (:gmtoff c) 0))) 3600) 60) 2))
     :OF (fn [c] (no-interval! c)
           (let [g (long (or (:gmtoff c) 0)) a (Math/abs g)]
             (str (if (>= g 0) "+" "-") (zero-pad (quot a 3600) (if (:fm? c) 0 2))
                  (when-not (zero? (rem a 3600)) (str ":" (zero-pad (quot (rem a 3600) 60) 2))))))
     :A_D (era "B.C." "A.D.") :B_C (era "B.C." "A.D.")
     :AD (era "BC" "AD") :BC (era "BC" "AD")
     :a_d (era "b.c." "a.d.") :b_c (era "b.c." "a.d.")
     :ad (era "bc" "ad") :bc (era "bc" "ad")
     :MONTH (month months-full :upper) :Month (month months-full nil) :month (month months-full :lower)
     :MON (mon str/upper-case) :Mon (mon identity) :mon (mon str/lower-case)
     :MM (fn [c] (num-out c (zero-pad (:mon c) (width c 2 (:mon c)))))
     :DAY (day :upper) :Day (day nil) :day (day :lower)
     :DY (dy str/upper-case) :Dy (dy identity) :dy (dy str/lower-case)
     :DDD (fn [c] (num-out c (zero-pad (:yday c) (if (:fm? c) 0 3))))
     :IDDD (fn [c] (num-out c (zero-pad (nth (iso c) 2) (if (:fm? c) 0 3))))
     :DD (fn [c] (num-out c (zero-pad (:mday c) (if (:fm? c) 0 2))))
     :D (fn [c] (no-interval! c) (num-out c (str (inc (long (:wday c))))))
     :ID (fn [c] (no-interval! c) (num-out c (str (if (zero? (long (:wday c))) 7 (:wday c)))))
     :WW (fn [c] (num-out c (zero-pad (inc (quot (dec (long (:yday c))) 7)) (if (:fm? c) 0 2))))
     :IW (fn [c] (num-out c (zero-pad (nth (iso c) 1) (if (:fm? c) 0 2))))
     :Q (fn [c] (when (pos? (long (:mon c))) (num-out c (str (inc (quot (dec (long (:mon c))) 3))))))
     :CC (fn [c] (let [year (long (:year c))
                       i (cond (:interval? c) (quot year 100)
                               (pos? year) (inc (quot (dec year) 100))
                               :else (dec (quot year 100)))]
                   (num-out c (if (<= -99 i 99) (zero-pad i (if (:fm? c) 0 (if (>= i 0) 2 3))) (str i)))))
     :Y_YYY (fn [c] (let [y (adj-year (:year c) (:interval? c)) i (quot y 1000)]
                      (num-out c (str i "," (zero-pad (- y (* i 1000)) 3)))))
     :YYYY (yyyy nil 4 :IYYY) :IYYY (yyyy nil 4 :IYYY)
     :YYY (yyyy 1000 3 :IYY) :IYY (yyyy 1000 3 :IYY)
     :YY (yyyy 100 2 :IY) :IY (yyyy 100 2 :IY)
     :Y (fn [c] (num-out c (str (rem (year-of c (:key c) :I) 10))))
     :I (fn [c] (num-out c (str (rem (year-of c (:key c) :I) 10))))
     :RM (rm rm-upper) :rm (rm rm-lower)
     :W (fn [c] (num-out c (str (inc (quot (dec (long (:mday c))) 7)))))
     :J (fn [c] (num-out c (str (date2j (:year c) (:mon c) (:mday c)))))}))

(defn to-char
  "DCH_to_char over `tm`, a map of the broken-down value:
   :year (astronomical) :mon :mday :hour :min :sec :usec, plus :yday and
   :wday for a timestamp, and :gmtoff / :tzn for a timestamptz.
   `interval?` selects the interval rules."
  [tm interval? ^String fmt]
  (let [base (assoc tm :interval? interval?
                    :usec (or (:usec tm) 0) :yday (or (:yday tm) 0) :wday (or (:wday tm) 0))
        sb (StringBuilder.)]
    (doseq [{:keys [key suffix text]} (parse-format fmt)]
      (if text
        (.append sb ^String text)
        (when-let [f (get renderers key)]
          (let [c (assoc base :key key
                         :fm? (contains? suffix :FM)
                         :tm? (contains? suffix :TM)
                         :th (cond (contains? suffix :TH) :upper (contains? suffix :th) :lower))]
            (when-let [s (f c)] (.append sb ^String s))))))
    (str sb)))

;; ---------------------------------------------------------------------------
;; Broken-down values

(defn- wall-fields [^java.time.LocalDateTime t]
  (let [y (.getYear t) m (.getMonthValue t) d (.getDayOfMonth t)
        j (date2j y m d)]
    {:year y :mon m :mday d
     :hour (.getHour t) :min (.getMinute t) :sec (.getSecond t)
     :usec (quot (.getNano t) 1000)
     :wday (mod (inc j) 7)
     :yday (inc (- j (date2j y 1 1)))}))

(defn timestamp-fields
  "A timestamp's fields: its wall clock as it stands."
  [^java.time.LocalDateTime t]
  (wall-fields t))

(defn timestamptz-fields
  "A timestamptz's fields: its wall clock in `zone`, with that zone's
   offset and abbreviation at the instant."
  [^java.time.Instant i ^java.time.ZoneId zone]
  (let [zdt (.atZone i zone)
        off (.getTotalSeconds (.getOffset zdt))
        abbrev (let [s (.format zdt (java.time.format.DateTimeFormatter/ofPattern "zzz" java.util.Locale/US))]
                 ;; Java names a fixed offset `+02:00` / `GMT+02:00`;
                 ;; PostgreSQL's abbreviation for one is `+02`.
                 (if (re-matches #"(?:GMT|UTC)?[+-]\d{2}:\d{2}" s)
                   (let [h (quot (Math/abs off) 3600) mi (quot (rem (Math/abs off) 3600) 60)]
                     (str (if (neg? off) "-" "+") (format "%02d" h) (when (pos? mi) (format ":%02d" mi))))
                   s))]
    (assoc (wall-fields (.toLocalDateTime zdt)) :gmtoff off :tzn abbrev)))

(defn interval-fields
  "interval2itm: years and months from the month count, days as they are,
   and the time split into hours (unbounded), minutes, seconds and
   microseconds, each carrying the sign."
  [^long months ^long days ^long micros]
  (let [year (quot months 12) mon (rem months 12)
        hour (quot micros 3600000000) r (rem micros 3600000000)
        min (quot r 60000000) r (rem r 60000000)
        sec (quot r 1000000) usec (rem r 1000000)]
    {:year year :mon mon :mday days :hour hour :min min :sec sec :usec usec
     :yday (+ (* (+ (* year 12) mon) 30) days)}))
