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
              ;; One node per character, as parse_format makes them: on
              ;; input each consumes one character.
              (recur (long k) (into out (map (fn [c] {:text (str c)})) text)))

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

;; ---------------------------------------------------------------------------
;; The input side: DCH_from_char and do_to_timestamp

(def ^:private date-mode
  "DCH_keywords' date_mode column: which convention a keyword belongs to."
  (merge (zipmap [:DDD :DD :D :MM :MONTH :MON :Month :Mon :month :mon :RM :rm :WW :W
                  :Y_YYY :YYYY :YYY :YY :Y] (repeat :gregorian))
         (zipmap [:IDDD :ID :IW :IYYY :IYY :IY :I] (repeat :isoweek))))

(def ^:private digit-keys
  "DCH_keywords' is_digit column."
  #{:CC :DDD :DD :D :FF1 :FF2 :FF3 :FF4 :FF5 :FF6 :HH24 :HH12 :HH :IDDD :ID :IW :IYYY
    :IYY :IY :I :J :MI :MM :MS :Q :SSSS :SS :TZM :US :WW :W :Y_YYY :YYYY :YYY :YY :Y})

(defn- dt-format-error
  ([msg] (dt-format-error msg nil nil))
  ([msg detail hint]
   (throw (ex-info msg (cond-> {:sqlstate "22007"}
                         detail (assoc :detail detail)
                         hint (assoc :hint hint))))))

(defn- field-overflow! [^String input]
  (throw (ex-info (str "date/time field value out of range: \"" input "\"")
                  {:sqlstate "22008"})))

(defn- space? [c] (Character/isWhitespace (char c)))

(defn- seq-search
  "seq_search_ascii: the first entry that the input starts with,
   case-insensitively. [index length] or nil."
  [^String s ^long i names]
  (some (fn [[idx ^String name]]
          (when (.regionMatches s true (int i) name 0 (count name))
            [idx (count name)]))
        (map-indexed vector names)))

(defn- next-separator?
  "is_next_separator: whether the node after `idx` cannot be digits, so a
   number may be read greedily."
  [nodes ^long idx]
  (let [n (nth nodes idx)]
    (cond
      (and (:key n) (some #{:TH :th} (:suffix n))) true
      :else (let [m (get nodes (inc idx))]
              (cond
                (nil? m) true
                (:key m) (not (contains? digit-keys (:key m)))
                :else (not (and (= 1 (count (:text m))) (Character/isDigit (.charAt ^String (:text m) 0)))))))))

(defn- parse-int-len
  "from_char_parse_int_len: [value new-position]."
  [^String s pos len node nodes idx]
  (let [pos (long pos) len (long len)
        n (count s)
        p (loop [p pos] (if (and (< p n) (space? (.charAt s p))) (recur (inc p)) p))
        copy (subs s p (min n (+ p len)))
        greedy? (or (contains? (:suffix node) :FM) (next-separator? nodes idx))
        strtol (fn [^String t]
                 ;; C's strtol: optional whitespace, sign, digits
                 (let [m (re-find #"^\s*[+-]?\d+" t)]
                   (when m [(Long/parseLong (str/trim m)) (count m)])))]
    (if greedy?
      (let [[v used] (or (strtol (subs s pos)) [nil 0])]
        (when-not v
          (dt-format-error (str "invalid value \"" copy "\" for \"" (:name node) "\"")
                           "Value must be an integer." nil))
        (when (or (> v Integer/MAX_VALUE) (< v Integer/MIN_VALUE))
          (throw (ex-info (str "value for \"" (:name node) "\" in source string is out of range")
                          {:sqlstate "22008"
                           :detail "Value must be in the range -2147483648 to 2147483647."})))
        [v (+ pos used)])
      (do
        (when (< (count copy) len)
          (dt-format-error (str "source string too short for \"" (:name node) "\" formatting field")
                           (format "Field requires %d characters, but only %d remain." len (count copy))
                           "If your source string is not fixed-width, try using the \"FM\" modifier."))
        (let [[v used] (or (strtol copy) [nil 0])]
          (when (and v (pos? used) (< used len))
            (dt-format-error (str "invalid value \"" copy "\" for \"" (:name node) "\"")
                             (format "Field requires %d characters, but only %d could be parsed." len used)
                             "If your source string is not fixed-width, try using the \"FM\" modifier."))
          (when-not v
            (dt-format-error (str "invalid value \"" copy "\" for \"" (:name node) "\"")
                             "Value must be an integer." nil))
          [v (+ p used)])))))

(defn- set-int!
  "from_char_set_int: a field may be set once, or again to the same value."
  [tmfc k v node]
  (let [old (get tmfc k 0)]
    (when (and (not= 0 old) (not= old v))
      (dt-format-error (str "conflicting values for \"" (:name node) "\" field in formatting string")
                       "This value contradicts a previous setting for the same field type." nil))
    (assoc tmfc k v)))

(defn- adjust-partial-year [^long y]
  (cond (< y 70) (+ y 2000) (< y 100) (+ y 1900) (< y 520) (+ y 2000) (< y 1000) (+ y 1000) :else y))

(defn- skip-th [s ^long pos suffix]
  (if (some #{:TH :th} suffix) (min (count s) (+ pos 2)) pos))

(defn- word-at
  "from_char_seq_search: [index length], or its error."
  [^String in pos names node]
  (or (seq-search in pos names)
      (let [rest-s (subs in pos)
            cut (or (re-find #"^\S*" rest-s) rest-s)]
        (dt-format-error (str "invalid value \"" cut "\" for \"" (:name node) "\"")
                         "The given value did not match any of the allowed values for this field." nil))))

(defn- int-field-at
  "from_char_parse_int[_len] into `field` (nil: parsed and dropped):
   [tmfc new-position characters-used value]."
  [tmfc in pos field len node nodes idx]
  (let [[v p] (parse-int-len in pos len node nodes idx)]
    [(if field (set-int! tmfc field v node) tmfc) p (- (long p) (long pos)) v]))

(defn- read-field
  "One keyword of DCH_from_char's switch: [tmfc new-position]."
  [tmfc ^String in pos extra node nodes idx]
  (let [k (:key node)
        n (count in)
        int-field (fn [field len] (int-field-at tmfc in pos field len node nodes idx))
        word (fn [names] (word-at in pos names node))]
    (case k
      :FX [(assoc tmfc ::fx true) pos]
      (:A_M :P_M :a_m :p_m) (let [[i l] (word ["a.m." "p.m." "A.M." "P.M."])]
                              [(-> (set-int! tmfc :pm (mod i 2) node) (assoc :clock 12)) (+ pos l)])
      (:AM :PM :am :pm) (let [[i l] (word ["am" "pm" "AM" "PM"])]
                          [(-> (set-int! tmfc :pm (mod i 2) node) (assoc :clock 12)) (+ pos l)])
      (:HH :HH12) (let [[t p] (int-field :hh 2)] [(assoc t :clock 12) (skip-th in p (:suffix node))])
      :HH24 (let [[t p] (int-field :hh 2)] [t (skip-th in p (:suffix node))])
      :MI (let [[t p] (int-field :mi 2)] [t (skip-th in p (:suffix node))])
      :SS (let [[t p] (int-field :ss 2)] [t (skip-th in p (:suffix node))])
      :MS (let [[t p used] (int-field :ms 3)]
            [(update t :ms * (case (long used) 1 100 2 10 1)) (skip-th in p (:suffix node))])
      (:FF1 :FF2 :FF3 :FF4 :FF5 :FF6 :US)
      (let [ff (case k :FF1 1 :FF2 2 :FF3 3 :FF4 4 :FF5 5 :FF6 6 nil)
            tmfc0 tmfc
            [v p] (parse-int-len in pos (or ff 6) node nodes idx)
            used (- p (long pos))
            t (cond-> (set-int! tmfc0 :us v node) ff (assoc :ff ff))]
        [(update t :us * (case (long used) 1 100000 2 10000 3 1000 4 100 5 10 1))
         (skip-th in p (:suffix node))])
      :SSSS (let [[t p] (int-field :ssss (count (:name node)))] [t (skip-th in p (:suffix node))])
      (:tz :TZ :OF)
      (let [tz-word (when (#{:tz :TZ} k)
                      (let [letters (re-find #"^[A-Za-z]{1,10}" (subs in pos))]
                        (when letters
                          (some (fn [l] (let [ab (subs (str/lower-case letters) 0 l)]
                                          (when-let [e (get @(requiring-resolve 'datahike.pg.datetime.tokens/zone-abbrevs) ab)]
                                            [l e])))
                                (range (count letters) 0 -1)))))]
        (cond
          tz-word
          (let [[l e] tz-word]
            [(-> tmfc (assoc :has-tz true :tz-abbrev e :tzsign 0)) (+ pos l)])
          (and (#{:tz :TZ} k) (Character/isLetter (.charAt in pos)))
          (dt-format-error (str "invalid value \"" (subs in pos) "\" for \"" (:name node) "\"")
                           "Time zone abbreviation is not recognized." nil)
          :else
          (let [c (.charAt in pos)
                [sign p] (if (#{\+ \- \space} c)
                           [(if (= c \-) -1 1) (inc pos)]
                           [(if (and (pos? extra) (= \- (.charAt in (dec pos)))) -1 1) pos])
                tmfc (assoc tmfc :tzsign sign)
                [h p] (parse-int-len in p 2 node nodes idx)
                tmfc (set-int! tmfc :tzh h node)]
            (if (and (< p n) (= \: (.charAt in p)))
              (let [[m p2] (parse-int-len in (inc p) 2 node nodes idx)]
                [(set-int! tmfc :tzm m node) p2])
              [tmfc p]))))
      :TZH (let [c (.charAt in pos)
                 [sign p] (if (#{\+ \- \space} c)
                            [(if (= c \-) -1 1) (inc pos)]
                            [(if (and (pos? extra) (= \- (.charAt in (dec pos)))) -1 1) pos])
                 [h p] (parse-int-len in p 2 node nodes idx)]
             [(-> tmfc (assoc :tzsign sign) (set-int! :tzh h node)) p])
      :TZM (let [tmfc (if (zero? (long (get tmfc :tzsign 0))) (assoc tmfc :tzsign 1) tmfc)
                 [m p] (parse-int-len in pos 2 node nodes idx)]
             [(set-int! tmfc :tzm m node) p])
      (:A_D :B_C :a_d :b_c) (let [[i l] (word ["a.d." "b.c." "A.D." "B.C."])]
                              [(set-int! tmfc :bc (mod i 2) node) (+ pos l)])
      (:AD :BC :ad :bc) (let [[i l] (word ["ad" "bc" "AD" "BC"])]
                          [(set-int! tmfc :bc (mod i 2) node) (+ pos l)])
      (:MONTH :Month :month) (let [[i l] (word months-full)] [(set-int! tmfc :mm (inc i) node) (+ pos l)])
      (:MON :Mon :mon) (let [[i l] (word months)] [(set-int! tmfc :mm (inc i) node) (+ pos l)])
      :MM (let [[t p] (int-field :mm 2)] [t (skip-th in p (:suffix node))])
      (:DAY :Day :day) (let [[i l] (word days)] [(set-int! tmfc :d (inc i) node) (+ pos l)])
      (:DY :Dy :dy) (let [[i l] (word days-short)] [(set-int! tmfc :d (inc i) node) (+ pos l)])
      :DDD (let [[t p] (int-field :ddd 3)] [t (skip-th in p (:suffix node))])
      :IDDD (let [[t p] (int-field :ddd 3)] [t (skip-th in p (:suffix node))])
      :DD (let [[t p] (int-field :dd 2)] [t (skip-th in p (:suffix node))])
      :D (let [[t p] (int-field :d 1)] [t (skip-th in p (:suffix node))])
      :ID (let [[v p] (parse-int-len in pos 1 node nodes idx)
                v (let [x (inc v)] (if (> x 7) 1 x))]
            [(set-int! tmfc :d v node) (skip-th in p (:suffix node))])
      (:WW :IW) (let [[t p] (int-field :ww 2)] [t (skip-th in p (:suffix node))])
      :Q (let [[_ p] (parse-int-len in pos 1 node nodes idx)] [tmfc (skip-th in p (:suffix node))])
      :CC (let [[t p] (int-field :cc 2)] [t (skip-th in p (:suffix node))])
      :Y_YYY (let [m (re-find #"^\s*([+-]?\d+),(\d{3})" (subs in pos))]
               (when-not m (dt-format-error "invalid input string for \"Y,YYY\""))
               (let [y (+ (* 1000 (Long/parseLong (nth m 1))) (Long/parseLong (nth m 2)))]
                 [(-> (set-int! tmfc :year y node) (assoc :yysz 4))
                  (skip-th in (+ pos (count (first m))) (:suffix node))]))
      (:YYYY :IYYY) (let [[t p] (int-field :year 4)] [(assoc t :yysz 4) (skip-th in p (:suffix node))])
      (:YYY :IYY :YY :IY :Y :I)
      (let [l (case k (:YYY :IYY) 3 (:YY :IY) 2 1)
            [v p] (parse-int-len in pos l node nodes idx)
            used (- p (long pos))
            v (if (< used 4) (adjust-partial-year v) v)]
        [(-> (set-int! tmfc :year v node) (assoc :yysz l)) (skip-th in p (:suffix node))])
      (:RM :rm) (let [[i l] (word rm-lower)] [(set-int! tmfc :mm (- 12 i) node) (+ pos l)])
      :W (let [[t p] (int-field :w 1)] [t (skip-th in p (:suffix node))])
      :J (let [[t p] (int-field :j 1)] [t (skip-th in p (:suffix node))])
      [tmfc pos])))

(defn from-char
  "DCH_from_char, non-standard mode: the TmFromChar fields as a map."
  [^String in ^String fmt]
  (let [nodes (vec (parse-format fmt))
        n (count in)
        skip-spaces (fn [p] (loop [p p k (identity 0)]
                              (if (and (< (long p) n) (space? (.charAt in (int p))))
                                (recur (inc (long p)) (inc (long k)))
                                [p k])))
        len-of {:HH 2 :HH12 2 :HH24 2 :MS 3 :US 6 :IDDD 3 :ID 1 :TZH 2 :TZM 2}]
    ;; Boxed loop locals (`identity`): primitive ones captured by the
    ;; closures in the body trip the compiler.
    (loop [idx (identity 0) pos (identity 0) extra (identity 0) fx? false tmfc {}]
      (if (or (>= idx (count nodes)) (>= pos n))
        tmfc
        (let [node (nth nodes idx)
              k (:key node)
              ;; skip leading spaces before a field (or at the start)
              [pos extra] (if (and (not fx?) (not= k :FX) (or k (zero? idx)))
                            (let [[p s] (skip-spaces pos)] [p (+ extra s)])
                            [pos extra])]
          (if (>= pos n)
            tmfc
            (cond
              ;; a literal: a space or separator in the format
              (and (nil? k) (let [t (:text node)] (and (= 1 (count t))
                                                       (or (space? (.charAt ^String t 0))
                                                           (separator-char? (.charAt ^String t 0))))))
              (if fx?
                (recur (inc idx) (inc pos) extra fx? tmfc)
                (let [c (.charAt in pos)
                      take? (or (space? c) (separator-char? c))]
                  (recur (inc idx) (if take? (inc pos) pos) (if take? extra (dec extra)) fx? tmfc)))

              (nil? k)
              ;; any other text character consumes one input character,
              ;; unless extra characters were skipped before it
              (if (and (not fx?) (pos? extra))
                (recur (inc idx) pos (dec extra) fx? tmfc)
                (recur (inc idx) (inc pos) extra fx? tmfc))

              :else
              (let [tmfc (if-let [m (date-mode k)]
                           (if (and (:mode tmfc) (not= m (:mode tmfc)))
                             (dt-format-error "invalid combination of date conventions" nil
                                              "Do not mix Gregorian and ISO week date conventions in a formatting template.")
                             (assoc tmfc :mode m))
                           tmfc)
                    [tmfc pos] (read-field tmfc in pos extra node nodes idx)
                    fx? (or fx? (boolean (::fx tmfc)))
                    ;; spaces after a field are ignored, and counted
                    [pos extra] (if fx? [pos extra] (skip-spaces pos))]
                (recur (inc idx) pos (if fx? extra extra) fx? tmfc)))))))))

(defn- j2date
  "PostgreSQL's j2date: [year month day] of a Julian day (astronomical
   year). java.time agrees with the C over the Julian range PostgreSQL
   accepts; 2440588 is the Julian day of 1970-01-01."
  [^long jd]
  (let [ld (java.time.LocalDate/ofEpochDay (- jd 2440588))]
    [(.getYear ld) (.getMonthValue ld) (.getDayOfMonth ld)]))

(defn- isoweek2j
  "isoweek2j: the Julian day of the Monday of ISO week `wk` of `year`."
  ^long [^long year ^long wk]
  (let [day4 (date2j year 1 4)
        day0 (mod (inc day4) 7)
        day0 (if (zero? day0) 7 day0)]
    (+ (- day4 (dec day0)) (* 7 (dec wk)))))

(defn- leap-year? [^long y] (and (zero? (mod y 4)) (or (pos? (mod y 100)) (zero? (mod y 400)))))

(def ^:private month-days-tab
  [[31 28 31 30 31 30 31 31 30 31 30 31] [31 29 31 30 31 30 31 31 30 31 30 31]])

(defn do-to-timestamp
  "do_to_timestamp: the input parsed by `fmt` into
   {:year :mon :mday :hour :min :sec :fsec :tz (seconds WEST, or nil) :ff}."
  [^String input ^String fmt]
  (let [tmfc (if (str/blank? fmt) {} (from-char input fmt))
        g #(long (get tmfc % 0))
        tm {:year 0 :mon 1 :mday 1 :hour 0 :min 0 :sec 0}
        tm (if (pos? (g :ssss))
             (let [x (g :ssss)]
               (assoc tm :hour (quot x 3600) :min (quot (rem x 3600) 60) :sec (rem x 60)))
             tm)
        tm (cond-> tm
             (not (zero? (g :ss))) (assoc :sec (g :ss))
             (not (zero? (g :mi))) (assoc :min (g :mi))
             (not (zero? (g :hh))) (assoc :hour (g :hh)))
        tm (if (= 12 (:clock tmfc))
             (let [h (long (:hour tm))]
               (when (or (< h 1) (> h 12))
                 (dt-format-error (str "hour \"" h "\" is invalid for the 12-hour clock") nil
                                  "Use the 24-hour clock, or give an hour between 1 and 12."))
               (assoc tm :hour (cond (and (= 1 (g :pm)) (< h 12)) (+ h 12)
                                     (and (zero? (g :pm)) (= h 12)) 0
                                     :else h)))
             tm)
        bc? (= 1 (g :bc))
        [tm ymask] (cond
                     (not (zero? (g :year)))
                     (if (and (not (zero? (g :cc))) (<= (g :yysz) 2))
                       (let [cc (if bc? (- (g :cc)) (g :cc))
                             y (rem (g :year) 100)
                             y (if (zero? y)
                                 (+ (* cc 100) (if (>= cc 0) 0 1))
                                 (if (>= cc 0) (+ y (* (dec cc) 100)) (+ (- (* (inc cc) 100) y) 1)))]
                         [(assoc tm :year y) true])
                       (let [y (g :year) y (if bc? (- y) y) y (if (neg? y) (inc y) y)]
                         [(assoc tm :year y) true]))
                     (not (zero? (g :cc)))
                     (let [cc (if bc? (- (g :cc)) (g :cc))]
                       [(assoc tm :year (if (>= cc 0) (inc (* (dec cc) 100)) (inc (* cc 100)))) true])
                     :else [tm false])
        fmask (atom (cond-> #{} ymask (conj :year)))
        tm (if (not (zero? (g :j)))
             (let [[y m d] (j2date (g :j))] (swap! fmask into [:year :mon :mday])
                  (assoc tm :year y :mon m :mday d))
             tm)
        tmfc (if (and (not (zero? (g :ww))) (not= :isoweek (:mode tmfc)))
               (assoc tmfc :ddd (inc (* 7 (dec (g :ww)))))
               tmfc)
        g #(long (get tmfc % 0))
        tm (if (and (not (zero? (g :ww))) (= :isoweek (:mode tmfc)))
             (let [j (if (not (zero? (g :d)))
                       ;; isoweekdate2date: day 1..7 with Sunday as 1 here
                       (let [wd (g :d) wd (if (= 1 wd) 7 (dec wd))]
                         (+ (isoweek2j (:year tm) (g :ww)) (dec wd)))
                       (isoweek2j (:year tm) (g :ww)))
                   [y m d] (j2date j)]
               (swap! fmask into [:year :mon :mday])
               (assoc tm :year y :mon m :mday d))
             tm)
        tmfc (if (not (zero? (g :w))) (assoc tmfc :dd (inc (* 7 (dec (g :w))))) tmfc)
        g #(long (get tmfc % 0))
        tm (cond-> tm
             (not (zero? (g :dd))) (do (swap! fmask conj :mday) (assoc tm :mday (g :dd))))
        tm (cond-> tm
             (not (zero? (g :mm))) (do (swap! fmask conj :mon) (assoc tm :mon (g :mm))))
        tm (if (and (not (zero? (g :ddd))) (or (<= (long (:mon tm)) 1) (<= (long (:mday tm)) 1)))
             (do
               (when (and (zero? (long (:year tm))) (not bc?))
                 (dt-format-error "cannot calculate day of year without year information"))
               (if (= :isoweek (:mode tmfc))
                 (let [j0 (dec (isoweek2j (:year tm) 1))
                       [y m d] (j2date (+ j0 (g :ddd)))]
                   (swap! fmask into [:year :mon :mday])
                   (assoc tm :year y :mon m :mday d))
                 (let [cum (reductions + 0 (month-days-tab (if (leap-year? (:year tm)) 1 0)))
                       i (or (first (filter #(<= (g :ddd) (nth cum %)) (range 1 13))) 13)]
                   (swap! fmask into [:mon :mday])
                   (cond-> tm
                     (<= (long (:mon tm)) 1) (assoc :mon i)
                     (<= (long (:mday tm)) 1) (assoc :mday (- (g :ddd) (nth cum (dec i) 0)))))))
             tm)
        fsec (+ (* 1000 (g :ms)) (g :us))
        m @fmask]
    ;; ValidateDate, Julian mode: the year is taken as it is.
    (when (seq m)
      (let [mon (long (:mon tm)) mday (long (:mday tm))]
        (when (or (and (contains? m :mon) (or (< mon 1) (> mon 12)))
                  (and (contains? m :mday) (or (< mday 1) (> mday 31)))
                  (and (every? m [:year :mon :mday])
                       (<= 1 mon 12)
                       (> mday (get-in month-days-tab [(if (leap-year? (:year tm)) 1 0) (dec mon)]))))
          (field-overflow! input))))
    (when (or (not (<= 0 (long (:hour tm)) 23)) (not (<= 0 (long (:min tm)) 59))
              (not (<= 0 (long (:sec tm)) 59)) (not (<= 0 fsec 999999)))
      (field-overflow! input))
    (let [tz (cond
               (not (zero? (g :tzsign)))
               (do (when (or (not (<= 0 (g :tzh) 15)) (not (<= 0 (g :tzm) 59)))
                     (throw (ex-info (str "time zone displacement out of range: \"" input "\"")
                                     {:sqlstate "22009"})))
                   (let [off (* 60 (+ (* 60 (g :tzh)) (g :tzm)))]
                     (if (pos? (g :tzsign)) (- off) off)))
               (:has-tz tmfc)
               (let [e (:tz-abbrev tmfc)]
                 (if (= :dyntz (:type e))
                   ((requiring-resolve 'datahike.pg.datetime.zone/determine-abbrev-offset) tm (:zone e))
                   (- (long (:offset e))))))]
      (assoc tm :fsec fsec :tz tz :ff (g :ff)))))

(defn- out-of-range! [msg]
  (throw (ex-info msg {:sqlstate "22008"})))

(defn to-timestamp
  "`to_timestamp(text, text)`: the parsed wall clock placed by the zone
   the input gave, else by `zone` (the session's), then rounded to the
   FFn precision if one was given (AdjustTimestampForTypmod, which rounds
   half away from zero measured from PostgreSQL's 2000-01-01 epoch)."
  ^java.time.OffsetDateTime [^String input ^String fmt ^java.time.ZoneId zone]
  (try
    (let [tm (try (do-to-timestamp input fmt)
                  (catch ArithmeticException _ (out-of-range! "timestamp out of range"))
                  (catch java.time.DateTimeException _ (out-of-range! "timestamp out of range")))
          west (long (or (:tz tm)
                         ((requiring-resolve 'datahike.pg.datetime.zone/determine-offset) tm zone)))
          local (try (java.time.LocalDateTime/of (int (:year tm)) (int (:mon tm)) (int (:mday tm))
                                                 (int (:hour tm)) (int (:min tm)) (int (:sec tm)))
                     (catch java.time.DateTimeException _ (out-of-range! "timestamp out of range")))
          epoch-2000 946684800
          us (+ (* 1000000 (+ (.toEpochSecond local java.time.ZoneOffset/UTC) west (- epoch-2000)))
                (long (:fsec tm)))
          ff (long (:ff tm))
          us (if (and (pos? ff) (< ff 6))
               (let [scale (long (Math/pow 10 (- 6 ff))) half (quot scale 2)]
                 (if (>= us 0)
                   (* (quot (+ us half) scale) scale)
                   (- (* (quot (+ (- us) half) scale) scale))))
               us)
          secs (+ epoch-2000 (Math/floorDiv us 1000000))
          nanos (* 1000 (Math/floorMod us 1000000))]
      (when (or (> (Math/abs (long (:year tm))) 294276))
        (out-of-range! "timestamp out of range"))
      (.atOffset (java.time.Instant/ofEpochSecond secs nanos) java.time.ZoneOffset/UTC))
    ;; a field too large for the arithmetic -- `to_timestamp('1500000000.5', 'J')`
    (catch ArithmeticException _ (out-of-range! "timestamp out of range"))))

(defn to-date
  "`to_date(text, text)`."
  ^java.time.LocalDate [^String input ^String fmt]
  (let [tm (try (do-to-timestamp input fmt)
                (catch ArithmeticException _ (out-of-range! (str "date out of range: \"" input "\"")))
                (catch java.time.DateTimeException _ (out-of-range! (str "date out of range: \"" input "\""))))
        y (long (:year tm))]
    ;; IS_VALID_JULIAN / IS_VALID_DATE
    (when (or (< y -4713) (> y 5874897))
      (out-of-range! (str "date out of range: \"" input "\"")))
    (java.time.LocalDate/of (int y) (int (:mon tm)) (int (:mday tm)))))
