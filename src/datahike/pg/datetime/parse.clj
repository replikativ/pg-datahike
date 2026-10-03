(ns datahike.pg.datetime.parse
  "`DecodeDateTime` (datetime.c:979-1534): the state machine that turns
   lexed fields into a date and time.

   It walks the fields once, left to right, and each field CLAIMS the
   parts of the result it set. A field that claims something already
   claimed is an error -- that single rule is what makes
   `'2001-02-03 +05 +06'` and `'Feb Feb 10 1997'` errors rather than
   last-one-wins, and it is why `fmask` and `tmask` are both needed:
   one is everything set so far, the other is what this field set.

   Two pieces of state carry ACROSS fields and are the reason this
   cannot be a fold over independent fields:

   `ptype` is a prefix set by a UNITS or ISOTIME token that changes how
   the NEXT field is read. `J` makes the next number a Julian day, `T`
   makes it a time. It must be consumed by the following field --
   a literal ending with a dangling `J` is an error.

   `text-month?` records that a month arrived as a word, which changes
   `DecodeNumber`'s placement rules and enables a retroactive swap.

   The zone is deliberately NOT resolved during the walk. An
   abbreviation, a zone name and the session default all need the
   DATE to resolve against -- an offset is a function of the instant --
   so the walk only records WHICH zone, and the resolution happens at
   the end, after `ValidateDate`. Getting this backwards resolves the
   zone against the wrong day across a DST boundary."
  (:require [clojure.set :as set]
            [datahike.pg.datetime.decode :as dec]
            [datahike.pg.datetime.lex :as lex]
            [datahike.pg.datetime.tokens :as tokens]
            [datahike.pg.datetime.zone :as zone]))

(defn- named-zone!
  "`pg_tzset(field[i])`, called DURING the walk as the C calls it --
   not deferred to resolution. The difference is observable: a
   timestamp discards its zone, so a zone validated only at resolution
   time is never validated at all, and `'2001-02-03 garbage'::timestamp`
   silently became 2001-02-03 instead of raising.

   Returns the ZONE to record. A POSIX name carries its own offset, so
   it comes back as `:offset` rather than `:named` -- nothing later has
   to know that `gmt+8` is special.

   `on-miss` is which error, and it is NOT the same for both callers.
   A zone arriving as a DATE field gives DTERR_BAD_TIMEZONE, 22023,
   `time zone ... not recognized` (datetime.c:1109). The same name
   arriving as a STRING gives plain DTERR_BAD_FORMAT, 22007
   (datetime.c:1434). So `Feb-10-1997` and `IDLW` fail with different
   SQLSTATEs -- the sort of distinction only the oracle would have
   told me about; I had both as 22023."
  [^String name on-miss]
  (if-let [west (zone/posix-offset name)]
    {:kind :offset :west west}
    (if (zone/resolve-zone-name name)
      {:kind :named :name name}
      ;; The ZONE goes in the error, not the whole literal: PostgreSQL
      ;; says `time zone "nonsense/zone" not recognized`, and the C
      ;; carries it in `extra->dtee_timezone` for exactly this.
      (throw (ex-info (clojure.core/name on-miss)
                      {::lex/dterr on-miss ::zone-name name})))))

(def ^:private hr24 :hr24)

(defn- clock!
  "The transaction timestamp, or a refusal.

   A caller that cannot supply one passes no `:now`, and the four
   clock-reading tokens then fail as unparseable -- which is what the
   cast path needs. Their value is the STATEMENT's, and a cast is
   constant-folded into a cached plan, so folding `'today'::date` once
   would freeze it for the life of that plan. Supporting them properly
   needs the deferred treatment `now()` gets; until then refusing is
   the honest answer and matches what the server already does."
  [now]
  (or now (dec/dterr :bad-format)))

(defn- claim
  "Merge `tmask` into `fmask`, refusing an overlap. The C's
   `if (tmask & fmask) return DTERR_BAD_FORMAT; fmask |= tmask;`"
  [fmask tmask]
  (when (seq (set/intersection fmask tmask)) (dec/dterr :bad-format))
  (into fmask tmask))

(defn- dt2time
  "`dt2time` (timestamp.c): microseconds since midnight to h/m/s/usec."
  [^long usec]
  (let [h (quot usec 3600000000)
        r (- usec (* h 3600000000))
        m (quot r 60000000)
        r (- r (* m 60000000))
        s (quot r 1000000)]
    {:hour h :min m :sec s :usec (- r (* s 1000000))}))

(defn- time-overflows?
  "`time_overflows` (date.c:1427-1444). TWO checks, and both are
   needed.

   The fields are range-checked individually first -- hour over 24,
   minute at or over 60, second over 60 -- which is what refuses
   `'1997.038'::time`: the run-together split makes that 19:97:00,
   whose total is under a day but whose minute field is not a minute.
   I had only the total check and we accepted it; the oracle gives
   22008.

   Then the TOTAL is checked separately, precisely because hour 24 and
   second 60 are individually legal: `23:59:60` and `24:00:00` are the
   same instant and both fine, while `24:00:00.001` is not."
  [{:keys [hour min sec usec]}]
  (or (neg? hour) (> hour 24)
      (neg? min) (>= min 60)
      (neg? sec) (> sec 60)
      (neg? usec) (> usec 1000000)
      (> (+ (* (long hour) 3600000000) (* (long min) 60000000)
            (* (long sec) 1000000) (long usec))
         86400000000)))

(defn- decode-special-field
  "One `:string` or `:special` field. `DecodeTimezoneAbbrev` is
   consulted BEFORE `DecodeSpecial` (datetime.c:1304-1309), because
   tzdb deliberately contains zone names identical to abbreviations."
  [text]
  (if-let [ab (tokens/zone-abbrev text)]
    (case (:type ab)
      :tz [:tz (:offset ab)]
      :dtz [:dtz (:offset ab)]
      :dyntz [:dyntz (:zone ab)])
    (tokens/special-token text)))

(defn decode-datetime
  "`DecodeDateTime`. Returns
   `{:dtype :date|:epoch|:late|:early :tm {…} :fields #{…} :zone …}`,
   or throws a `dterr`.

   `ctx` supplies what the C reads from global state: `:date-order`
   (DateStyle), `:now` (the transaction timestamp, for `now`/`today`),
   `:zone?` (whether this caller accepts a zone at all -- the C's
   `tzp != NULL`, which is how `date_in` refuses `'J2451187 +05'`).

   The ZONE is returned unresolved, as `{:kind :offset|:abbrev|:named
   …}`, because resolving it needs the validated date."
  [fields {:keys [date-order now zone?] :as ctx}]
  (loop [[{:keys [text type] :as f} & more] fields
         st {:fmask #{} :tm {:hour 0 :min 0 :sec 0 :usec 0}
             :dtype :date :ptype nil :mer hr24
             :text-month? false :julian? false :two-digits? false :bc? false
             :zone nil :isdst -1}]
    (if (nil? f)
      ;; --- after the walk ---
      (let [{:keys [fmask tm dtype ptype mer zone]} st]
        (when ptype (dec/dterr :bad-format))
        (if (not= dtype :date)
          {:dtype dtype :tm tm :fields fmask :zone zone}
          (let [tm (dec/validate-date tm fmask (select-keys st [:julian? :two-digits? :bc?]))
                ;; AM/PM, applied AFTER ValidateDate.
                _ (when (and (not= mer hr24) (> (:hour tm) 12))
                    (dec/dterr :field-overflow))
                tm (cond
                     (and (= mer :am) (= (:hour tm) 12)) (assoc tm :hour 0)
                     (and (= mer :pm) (not= (:hour tm) 12))
                     (update tm :hour + 12)
                     :else tm)]
            (when (not= (set/intersection fmask dec/date-fields) dec/date-fields)
              ;; A complete TIME with no date is `return 1` in the C
              ;; (datetime.c:1524) -- non-zero, so every caller of
              ;; DecodeDateTime treats it as an error. It exists as a
              ;; distinct value for callers that want a time, and none
              ;; of ours do.
              (dec/dterr :bad-format))
            {:dtype :date :tm tm :fields fmask
             :zone (or zone (when zone? {:kind :session}))})))

      ;; --- one field ---
      (let [{:keys [fmask tm ptype]} st
            r (case type
                :date (cond
                        (= ptype :julian)
                        (do (when-not zone? (dec/dterr :bad-format))
                            (let [[jday cp] (#'dec/strtoint text 0)]
                              (when (neg? jday) (dec/dterr :field-overflow))
                              (let [[y m d] (dec/j2date jday)
                                    tz (dec/decode-timezone (subs text cp))]
                                {:tm (merge tm {:year y :mon m :mday d})
                                 :tmask (into (into dec/date-fields dec/time-fields) [:tz])
                                 :julian? true :ptype nil
                                 :zone {:kind :offset :west tz}})))

                        ;; A trailing field after month and day are both
                        ;; known is a ZONE, not a date -- which is how
                        ;; `'Feb 10 1997 PST'` and `'… America/New_York'`
                        ;; are told apart from a second date.
                        (or ptype (set/subset? #{:month :day} fmask))
                        (do
                          (when-not zone? (dec/dterr :bad-format))
                          (if (or (Character/isDigit (.charAt ^String text 0)) ptype)
                            (let [_ (when ptype
                                      (when (not= ptype :time) (dec/dterr :bad-format)))
                                  _ (when (= (set/intersection fmask dec/time-fields)
                                             dec/time-fields)
                                      (dec/dterr :bad-format))
                                  i (.indexOf ^String text (int \-))
                                  _ (when (neg? i) (dec/dterr :bad-format))
                                  tz (dec/decode-timezone (subs text i))
                                  nf (dec/decode-number-field (subs text 0 i) fmask)]
                              {:tm (merge tm (:tm nf))
                               :tmask (conj (:tmask nf) :tz)
                               :two-digits? (:two-digits? nf) :ptype nil
                               :zone {:kind :offset :west tz}})
                            {:tmask #{:tz} :zone (named-zone! text :bad-timezone)}))

                        :else
                        (let [d (dec/decode-date text fmask ctx)]
                          {:tm (merge tm (:tm d))
                           :tmask (set/difference (:fields d) fmask)
                           :two-digits? (:two-digits? d)
                           :text-month? (or (:text-month? d) (:text-month? st))}))

                :time (do
                        (when ptype
                          (when (not= ptype :time) (dec/dterr :bad-format)))
                        (let [t (dec/decode-time text)]
                          (when (time-overflows? t) (dec/dterr :field-overflow))
                          {:tm (merge tm t) :tmask dec/time-fields :ptype nil}))

                :tz (do (when-not zone? (dec/dterr :bad-format))
                        {:tmask #{:tz}
                         :zone {:kind :offset :west (dec/decode-timezone text)}})

                :number
                (if ptype
                  (let [[value cp] (#'dec/strtoint text 0)
                        n (.length ^String text)
                        c (when (< cp n) (.charAt ^String text cp))]
                    (when-not (or (nil? c) (= c \.)) (dec/dterr :bad-format))
                    (case ptype
                      :julian
                      (do (when (neg? value) (dec/dterr :field-overflow))
                          (let [[y m d] (dec/j2date value)
                                base {:tm (merge tm {:year y :mon m :mday d})
                                      :tmask dec/date-fields
                                      :julian? true :ptype nil :dtype :date}]
                            (if (= c \.)
                              ;; A fraction of a Julian DAY, not of a
                              ;; second.
                              (let [frac (#'dec/parse-fraction text cp)
                                    t (dt2time (Math/round (* frac 86400000000.0)))]
                                (-> base
                                    (update :tm merge t)
                                    (assoc :tmask (into dec/date-fields dec/time-fields))))
                              base)))

                      :time
                      (let [nf (dec/decode-number-field text (into fmask dec/date-fields))]
                        (when (not= (:tmask nf) dec/time-fields) (dec/dterr :bad-format))
                        {:tm (merge tm (:tm nf)) :tmask (:tmask nf)
                         :two-digits? (:two-digits? nf) :ptype nil :dtype :date})

                      (dec/dterr :bad-format)))

                  ;; No prefix: four readings, and the ORDER of the
                  ;; tests is what picks between them.
                  (let [flen (.length ^String text)
                        dot (.indexOf ^String text (int \.))
                        have-date? (= (set/intersection fmask dec/date-fields)
                                      dec/date-fields)
                        have-time? (= (set/intersection fmask dec/time-fields)
                                      dec/time-fields)]
                    (cond
                      ;; A point with NO date yet means `yy.ddd`.
                      (and (>= dot 0) (not have-date?))
                      (let [d (dec/decode-date text fmask ctx)]
                        {:tm (merge tm (:tm d))
                         :tmask (set/difference (:fields d) fmask)
                         :two-digits? (:two-digits? d)})

                      ;; A point more than two digits in: a
                      ;; run-together number with a fraction.
                      (and (>= dot 0) (> dot 2))
                      (let [nf (dec/decode-number-field text fmask)]
                        {:tm (merge tm (:tm nf)) :tmask (:tmask nf)
                         :two-digits? (:two-digits? nf)})

                      (and (>= flen 6) (or (not have-date?) (not have-time?)))
                      (let [nf (dec/decode-number-field text fmask)]
                        {:tm (merge tm (:tm nf)) :tmask (:tmask nf)
                         :two-digits? (:two-digits? nf)})

                      :else
                      (let [nr (dec/decode-number text tm fmask
                                                  (assoc ctx
                                                         :text-month? (:text-month? st)
                                                         :two-digits? (:two-digits? st)))]
                        {:tm (merge tm (:tm nr)) :tmask (:tmask nr)
                         :two-digits? (:two-digits? nr)}))))

                (:string :special)
                (let [[kind val] (decode-special-field text)]
                  (case kind
                    nil ;; UNKNOWN_FIELD: the last chance is a zone NAME.
                    (do (when-not zone? (dec/dterr :bad-format))
                        {:tmask #{:tz} :zone (named-zone! text :bad-format)})

                    :ignore-dtf :skip

                    :reserv
                    (case val
                      :now (let [n (clock! now)]
                             {:tmask (into (into dec/date-fields dec/time-fields) [:tz])
                              :dtype :date :tm (merge tm (:tm n))
                              :zone {:kind :offset :west (:west n)}})
                      :today {:tmask dec/date-fields :dtype :date
                              :tm (merge tm (select-keys (:tm (clock! now))
                                                         [:year :mon :mday]))}
                      :yesterday
                      (let [{:keys [year mon mday]} (:tm (clock! now))
                            [y m d] (dec/j2date (dec (dec/date2j year mon mday)))]
                        {:tmask dec/date-fields :dtype :date
                         :tm (merge tm {:year y :mon m :mday d})})
                      :tomorrow
                      (let [{:keys [year mon mday]} (:tm (clock! now))
                            [y m d] (dec/j2date (inc (dec/date2j year mon mday)))]
                        {:tmask dec/date-fields :dtype :date
                         :tm (merge tm {:year y :mon m :mday d})})
                      :zulu {:tmask (conj dec/time-fields :tz) :dtype :date
                             :tm (merge tm {:hour 0 :min 0 :sec 0 :usec 0})
                             :zone (when zone? {:kind :offset :west 0})}
                      (:epoch :late :early)
                      {:tmask (into (into dec/date-fields dec/time-fields) [:tz])
                       :dtype val})

                    :month
                    ;; The month-to-day substitution: a SECOND month
                    ;; token, with the first having been numeric and no
                    ;; day set, means the first was really the day. That
                    ;; is what reads `'Feb 10 1997'` after `'10 Feb'`
                    ;; has already put 10 in the month slot.
                    (let [swap? (and (contains? fmask :month)
                                     (not (:text-month? st))
                                     (not (contains? fmask :day))
                                     (>= (:mon tm) 1) (<= (:mon tm) 31))]
                      {:tm (cond-> (assoc tm :mon val) swap? (assoc :mday (:mon tm)))
                       :tmask (if swap? #{:day} #{:month})
                       :text-month? true})

                    :dtzmod
                    (do (when-not zone? (dec/dterr :bad-format))
                        {:tmask #{:dtz} :isdst 1
                         :zone (update (or (:zone st) {:kind :offset :west 0})
                                       :west - val)})

                    ;; `tmask = DTK_M(type)` is assigned BEFORE the
                    ;; switch, so DTZ claims both DTZ and TZ while TZ
                    ;; claims only TZ.
                    :dtz (do (when-not zone? (dec/dterr :bad-format))
                             {:tmask #{:dtz :tz} :isdst 1
                              :zone {:kind :offset :west (- val)}})
                    :tz (do (when-not zone? (dec/dterr :bad-format))
                            {:tmask #{:tz} :isdst 0
                             :zone {:kind :offset :west (- val)}})
                    :dyntz (do (when-not zone? (dec/dterr :bad-format))
                               {:tmask #{:tz}
                                :zone {:kind :abbrev :zone val :abbrev text}})

                    :ampm {:tmask #{:ampm} :mer val}
                    :adbc {:tmask #{:adbc} :bc? (= val :bc)}
                    :dow {:tmask #{:dow} :tm (assoc tm :wday val)}
                    :units (do (when ptype (dec/dterr :bad-format))
                               {:tmask #{} :ptype val})
                    :isotime
                    (do (when (not= (set/intersection fmask dec/date-fields)
                                    dec/date-fields)
                          (dec/dterr :bad-format))
                        (when ptype (dec/dterr :bad-format))
                        {:tmask #{} :ptype val})
                    (dec/dterr :bad-format)))

                (dec/dterr :bad-format))]
        (if (= r :skip)
          (recur more st)
          (recur more (-> st
                          (merge (dissoc r :tmask))
                          (assoc :fmask (claim fmask (:tmask r)))
                          ;; `ptype` is cleared only by the fields that
                          ;; say so; it must survive the ones that do not.
                          (assoc :ptype (if (contains? r :ptype)
                                          (:ptype r) ptype)))))))))

(defn decode-time-only
  "`DecodeTimeOnly` (datetime.c:1838-2216): the same fields, read as a
   TIME.

   This is a separate function from `decode-datetime` in PostgreSQL and
   it stays separate here. The two look similar enough to invite
   factoring and they differ in a dozen places, every one of which
   changes an answer:

   The number thresholds are different. `DecodeNumberField` is reached
   at `flen > 4` here and at `flen >= 6` there, and every call passes
   `fmask | DTK_DATE_M` -- pretending the date is already complete. That
   is what makes `'040506'` a TIME to this function and a DATE to the
   other, from identical input.

   A leading `:date` field is only read as a date when it is first AND
   the field list looks like a date followed by a time -- the C tests
   `i == 0 && nf >= 2 && (ftype[nf-1] == DTK_DATE || ftype[1] ==
   DTK_TIME)`, a positional test on the OTHER fields. Otherwise it is a
   zone.

   Only two RESERV words are allowed: `now` and `allballs`. `today`,
   `epoch` and `infinity` are format errors for a time -- there is no
   infinite time of day.

   There is no MONTH case and no DOW case, so a month name or a weekday
   is a format error rather than being read or dropped.

   `time_overflows` is checked ONCE at the end, after AM/PM, rather
   than per field.

   And the zone resolution differs: a named zone with a FIXED offset
   needs no date, while one with DST rules does -- `'12:00 UTC'` is
   fine and `'12:00 America/New_York'` is not, because the offset is
   not a function of the time alone."
  [fields {:keys [date-order now zone?] :as ctx}]
  (let [nf (count fields)
        ftypes (mapv :type fields)]
    (loop [i 0
           [{:keys [text type] :as f} & more] fields
           st {:fmask #{} :tm {:hour 0 :min 0 :sec 0 :usec 0}
               :dtype :time :ptype nil :mer hr24
               :julian? false :two-digits? false :bc? false
               :zone nil :isdst -1}]
      (if (nil? f)
        (let [{:keys [fmask tm ptype mer zone]} st]
          (when ptype (dec/dterr :bad-format))
          (let [tm (dec/validate-date tm fmask
                                      (select-keys st [:julian? :two-digits? :bc?]))
                _ (when (and (not= mer hr24) (> (:hour tm) 12))
                    (dec/dterr :field-overflow))
                tm (cond
                     (and (= mer :am) (= (:hour tm) 12)) (assoc tm :hour 0)
                     (and (= mer :pm) (not= (:hour tm) 12)) (update tm :hour + 12)
                     :else tm)]
            (when (time-overflows? tm) (dec/dterr :field-overflow))
            (when (not= (set/intersection fmask dec/time-fields) dec/time-fields)
              (dec/dterr :bad-format))
            ;; A zone that is not a plain offset needs a DATE to resolve
            ;; against. With no date at all the CURRENT date is used;
            ;; with a partial one it is an error.
            (let [has-date? (= (set/intersection fmask dec/date-fields) dec/date-fields)
                  partial-date? (and (not has-date?)
                                     (seq (set/intersection fmask dec/date-fields)))
                  zone (or zone (when zone? {:kind :session}))]
              (when (and zone (not= (:kind zone) :offset) partial-date?)
                (dec/dterr :bad-format))
              {:dtype (:dtype st) :tm tm :fields fmask :zone zone
               ;; A DELAY, because reading the clock must not be
               ;; forced by parsing. `time_in` decodes a zone and then
               ;; discards it, so `'04:05:06'::time` reaches here with
               ;; a `:session` zone it will never resolve -- and a
               ;; caller that supplies no clock (the cast path, which
               ;; must not fold a volatile value into a cached plan)
               ;; would have been refused for an ordinary time.
               ;; The C reads it eagerly; it has a clock to read.
               :resolve-date (delay
                               (if has-date?
                                 (select-keys tm [:year :mon :mday])
                                 (select-keys (:tm (clock! now))
                                              [:year :mon :mday])))})))

        (let [{:keys [fmask tm ptype]} st
              r (case type
                  :date
                  (do
                    (when-not zone? (dec/dterr :bad-format))
                    (if (and (zero? i) (>= nf 2)
                             (or (= (peek ftypes) :date) (= (nth ftypes 1) :time)))
                      (let [d (dec/decode-date text fmask ctx)]
                        {:tm (merge tm (:tm d))
                         :tmask (set/difference (:fields d) fmask)
                         :two-digits? (:two-digits? d)})
                      (if (Character/isDigit (.charAt ^String text 0))
                        (let [_ (when (= (set/intersection fmask dec/time-fields)
                                         dec/time-fields)
                                  (dec/dterr :bad-format))
                              j (.indexOf ^String text (int \-))
                              _ (when (neg? j) (dec/dterr :bad-format))
                              tz (dec/decode-timezone (subs text j))
                              nfd (dec/decode-number-field
                                   (subs text 0 j) (into fmask dec/date-fields))]
                          {:tm (merge tm (:tm nfd)) :tmask (conj (:tmask nfd) :tz)
                           :two-digits? (:two-digits? nfd)
                           :zone {:kind :offset :west tz}})
                        {:tmask #{:tz} :zone (named-zone! text :bad-timezone)})))

                  :time (do
                          (when ptype
                            (when (not= ptype :time) (dec/dterr :bad-format)))
                          {:tm (merge tm (dec/decode-time text))
                           :tmask dec/time-fields :ptype nil})

                  :tz (do (when-not zone? (dec/dterr :bad-format))
                          {:tmask #{:tz}
                           :zone {:kind :offset :west (dec/decode-timezone text)}})

                  :number
                  (if ptype
                    (let [[value cp] (#'dec/strtoint text 0)
                          n (.length ^String text)
                          c (when (< cp n) (.charAt ^String text cp))]
                      (when-not (or (nil? c) (= c \.)) (dec/dterr :bad-format))
                      (case ptype
                        :julian
                        (do (when-not zone? (dec/dterr :bad-format))
                            (when (neg? value) (dec/dterr :field-overflow))
                            (let [[y m d] (dec/j2date value)
                                  base {:tm (merge tm {:year y :mon m :mday d})
                                        :tmask dec/date-fields
                                        :julian? true :ptype nil :dtype :date}]
                              (if (= c \.)
                                (let [frac (#'dec/parse-fraction text cp)]
                                  (-> base
                                      (update :tm merge
                                              (dt2time (Math/round (* frac 86400000000.0))))
                                      (assoc :tmask (into dec/date-fields dec/time-fields))))
                                base)))
                        :time
                        (let [nfd (dec/decode-number-field
                                   text (into fmask dec/date-fields))]
                          (when (not= (:tmask nfd) dec/time-fields)
                            (dec/dterr :bad-format))
                          {:tm (merge tm (:tm nfd)) :tmask (:tmask nfd)
                           :two-digits? (:two-digits? nfd) :ptype nil :dtype :date})
                        (dec/dterr :bad-format)))

                    (let [flen (.length ^String text)
                          dot (.indexOf ^String text (int \.))]
                      (cond
                        (>= dot 0)
                        (cond
                          (and (zero? i) (>= nf 2) (= (peek ftypes) :date))
                          (let [d (dec/decode-date text fmask ctx)]
                            {:tm (merge tm (:tm d))
                             :tmask (set/difference (:fields d) fmask)
                             :two-digits? (:two-digits? d)})
                          (> dot 2)
                          (let [nfd (dec/decode-number-field
                                     text (into fmask dec/date-fields))]
                            {:tm (merge tm (:tm nfd)) :tmask (:tmask nfd)
                             :two-digits? (:two-digits? nfd)})
                          :else (dec/dterr :bad-format))

                        ;; `> 4`, not `>= 6`. This is the line that makes
                        ;; `040506` a time.
                        (> flen 4)
                        (let [nfd (dec/decode-number-field
                                   text (into fmask dec/date-fields))]
                          {:tm (merge tm (:tm nfd)) :tmask (:tmask nfd)
                           :two-digits? (:two-digits? nfd)})

                        :else
                        (let [nr (dec/decode-number
                                  text tm (into fmask dec/date-fields)
                                  (assoc ctx :text-month? false
                                         :two-digits? (:two-digits? st)))]
                          {:tm (merge tm (:tm nr)) :tmask (:tmask nr)
                           :two-digits? (:two-digits? nr)}))))

                  (:string :special)
                  (let [[kind val] (decode-special-field text)]
                    (case kind
                      nil (do (when-not zone? (dec/dterr :bad-format))
                              {:tmask #{:tz} :zone (named-zone! text :bad-format)})
                      :ignore-dtf :skip
                      :reserv
                      (case val
                        :now {:tmask dec/time-fields :dtype :time
                              :tm (merge tm (select-keys (:tm (clock! now))
                                                         [:hour :min :sec :usec]))}
                        :zulu {:tmask (conj dec/time-fields :tz) :dtype :time
                               :tm (merge tm {:hour 0 :min 0 :sec 0 :usec 0})
                               :isdst 0
                               :zone (when zone? {:kind :offset :west 0})}
                        ;; today, tomorrow, epoch, infinity: there is no
                        ;; infinite time of day.
                        (dec/dterr :bad-format))
                      :dtzmod (do (when-not zone? (dec/dterr :bad-format))
                                  {:tmask #{:dtz} :isdst 1
                                   :zone (update (or (:zone st) {:kind :offset :west 0})
                                                 :west - val)})
                      :dtz (do (when-not zone? (dec/dterr :bad-format))
                               {:tmask #{:dtz :tz} :isdst 1
                                :zone {:kind :offset :west (- val)}})
                      :tz (do (when-not zone? (dec/dterr :bad-format))
                              {:tmask #{:tz} :isdst 0
                               :zone {:kind :offset :west (- val)}})
                      :dyntz (do (when-not zone? (dec/dterr :bad-format))
                                 {:tmask #{:tz}
                                  :zone {:kind :abbrev :zone val :abbrev text}})
                      :ampm {:tmask #{:ampm} :mer val}
                      :adbc {:tmask #{:adbc} :bc? (= val :bc)}
                      :units (do (when ptype (dec/dterr :bad-format))
                                 {:tmask #{} :ptype val})
                      :isotime (do (when ptype (dec/dterr :bad-format))
                                   {:tmask #{} :ptype val})
                      ;; No MONTH case and no DOW case: a month name or
                      ;; a weekday is an error in a time literal.
                      (dec/dterr :bad-format)))

                  (dec/dterr :bad-format))]
          (if (= r :skip)
            (recur (inc i) more st)
            (recur (inc i) more
                   (-> st
                       (merge (dissoc r :tmask))
                       (assoc :fmask (claim fmask (:tmask r)))
                       (assoc :ptype (if (contains? r :ptype) (:ptype r) ptype))))))))))
