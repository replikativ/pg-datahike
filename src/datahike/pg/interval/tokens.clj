(ns datahike.pg.interval.tokens
  "`deltatktbl` (datetime.c:187-251), PostgreSQL's INTERVAL unit table.

   A THIRD token table, beside `datetime/tokens.clj`'s two. It is not a
   superset of either and the three disagree on purpose:

     `m`   is MINUTE here and MONTH in `datetktbl`
     `mm`  is only in `datetktbl`
     `j`   is only in `datetktbl`

   And the disagreement is REACHABLE, because `DecodeInterval`'s
   DTK_STRING arm tries `DecodeUnits` (this table) and then FALLS BACK
   to `DecodeSpecial` (`datetktbl`) -- datetime.c:3659-3663. So `'1 m'`
   is one minute and `'1 mm'` is also one minute, by two different
   routes. Three tables, consulted in a fixed order.

   A PLAIN MAP LOOKUP IS WRONG HERE, unlike for the two datetime
   tables where `datetime.tokens/prefix-safe?` holds. `datebsearch`
   compares with `strncmp(key, token, TOKMAXLEN)` and TOKMAXLEN is 10,
   and FIVE keys in this table are exactly ten characters:

     microsecon  millennium  millisecon  timezone_h  timezone_m

   so each is a genuine PREFIX. Checked on the oracle:
   `'1 millenniumXYZ'::interval` is 1000 years and
   `'2 microsecondsXX'::interval` is 00:00:00.000002.

   FOUR UNITS RESOLVE HERE AND ARE STILL ERRORS. `DTK_QUARTER` and the
   three timezone units have no arm in `DecodeInterval`'s switch
   (datetime.c:3570-3650), so they reach its `default` and raise
   22007. `'1 qtr'`, `'1 quarter'`, `'1 timezone'`,
   `'1 timezone_hour'` and `'1 timezone_minute'` are all invalid
   intervals even though the lookup succeeds. A table-driven decoder
   that trusts this table would accept every one of them.

   GENERATED -- the extraction command is in the comment below."
  (:require [datahike.pg.datetime.tokens :as dt]))

;; Regenerating after a PostgreSQL bump, from .internal/postgres-REL_<ref>:
;;
;;   sed -n '/^static const datetkn deltatktbl\[\] = {/,/^};/p' \
;;     src/backend/utils/adt/datetime.c \
;;   | grep -oE '^[[:space:]]*\{[^}]*\}' \
;;   | sed 's/^[[:space:]]*{//; s/}$//; s/"//g; s/,[[:space:]]*/\t/g' \
;;   | awk -F'\t' 'BEGIN{
;;       m["DAGO"]="ago"; m["DMICROSEC"]="usecond"; m["DMILLISEC"]="msecond";
;;       m["DSECOND"]="second"; m["DMINUTE"]="minute"; m["DHOUR"]="hour";
;;       m["DDAY"]="day"; m["DWEEK"]="week"; m["DMONTH"]="month";
;;       m["DQUARTER"]="quarter"; m["DYEAR"]="year"; m["DDECADE"]="decade";
;;       m["DCENTURY"]="century"; m["DMILLENNIUM"]="millennium";
;;       m["DTIMEZONE"]="timezone" }
;;     {t=$1; if (t in m) t=m[t]; print t "\t" $2 "\t" $3}'
;;
;; FIFTEEN of the 61 tokens are spelled as macros in the C
;; (datetime.h:35-62); the awk resolves them. Without that they would
;; transcribe as the macro NAMES and `day`, `hour`, `month`, `year`,
;; `ago` and ten more would stop parsing entirely.

(def deltatktbl
  "61 entries: {token -> [type value]}. Keys are LOWERCASE, as
   `ParseDateTime` leaves them."
  {"@"            [:ignore-dtf  0]
   "ago"          [:ago         0]
   "c"            [:units       :century]
   "cent"         [:units       :century]
   "centuries"    [:units       :century]
   "century"      [:units       :century]
   "d"            [:units       :day]
   "day"          [:units       :day]
   "days"         [:units       :day]
   "dec"          [:units       :decade]
   "decade"       [:units       :decade]
   "decades"      [:units       :decade]
   "decs"         [:units       :decade]
   "h"            [:units       :hour]
   "hour"         [:units       :hour]
   "hours"        [:units       :hour]
   "hr"           [:units       :hour]
   "hrs"          [:units       :hour]
   "m"            [:units       :minute]
   "microsecon"   [:units       :microsec]
   "mil"          [:units       :millennium]
   "millennia"    [:units       :millennium]
   "millennium"   [:units       :millennium]
   "millisecon"   [:units       :millisec]
   "mils"         [:units       :millennium]
   "min"          [:units       :minute]
   "mins"         [:units       :minute]
   "minute"       [:units       :minute]
   "minutes"      [:units       :minute]
   "mon"          [:units       :month]
   "mons"         [:units       :month]
   "month"        [:units       :month]
   "months"       [:units       :month]
   "ms"           [:units       :millisec]
   "msec"         [:units       :millisec]
   "msecond"      [:units       :millisec]
   "mseconds"     [:units       :millisec]
   "msecs"        [:units       :millisec]
   "qtr"          [:units       :quarter]
   "quarter"      [:units       :quarter]
   "s"            [:units       :second]
   "sec"          [:units       :second]
   "second"       [:units       :second]
   "seconds"      [:units       :second]
   "secs"         [:units       :second]
   "timezone"     [:units       :tz]
   "timezone_h"   [:units       :tz-hour]
   "timezone_m"   [:units       :tz-minute]
   "us"           [:units       :microsec]
   "usec"         [:units       :microsec]
   "usecond"      [:units       :microsec]
   "useconds"     [:units       :microsec]
   "usecs"        [:units       :microsec]
   "w"            [:units       :week]
   "week"         [:units       :week]
   "weeks"        [:units       :week]
   "y"            [:units       :year]
   "year"         [:units       :year]
   "years"        [:units       :year]
   "yr"           [:units       :year]
   "yrs"          [:units       :year]})

(def ^:const tokmaxlen
  "TOKMAXLEN. `datebsearch` compares at most this many characters."
  10)

(def ^:private prefix-keys
  "The keys that are exactly `tokmaxlen` long, and therefore match any
   input beginning with them. Computed rather than listed, so a
   PostgreSQL bump that adds a sixth cannot silently leave it out."
  (into #{} (filter #(= (count %) tokmaxlen)) (keys deltatktbl)))

(defn decode-units
  "`DecodeUnits` (datetime.c:4047-4068): an interval unit word to
   `[type value]`, or nil.

   `strncmp(key, token, 10)` semantics exactly: an entry matches when
   it EQUALS the key, or when the entry is a full ten characters and
   the key starts with it. A shorter entry cannot prefix-match, because
   its NUL terminator falls inside the ten compared bytes -- which is
   why `days` has to be its own entry rather than matching `day`.

   `s` must already be ASCII-lowercased, as the lexer leaves it."
  [^String s]
  (or (get deltatktbl s)
      (when (> (count s) tokmaxlen)
        (let [p (subs s 0 tokmaxlen)]
          (when (contains? prefix-keys p) (get deltatktbl p))))))

(defn unit-or-special
  "`DecodeInterval`'s DTK_STRING/DTK_SPECIAL resolution, whole
   (datetime.c:3659-3663): this table FIRST, then `datetktbl`.

   The order is load-bearing and the two tables disagree -- `m` is a
   minute by this route and a month by the other -- so a caller must
   not consult them in the other order or merge them."
  [^String s]
  (or (decode-units s) (dt/special-token s)))
