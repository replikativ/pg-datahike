(ns datahike.pg.datetime.tokens
  "PostgreSQL's datetime token tables, transcribed from the pinned 17.7.

   TWO tables, and they must stay two. `datetktbl` (datetime.c:105-179)
   is compiled into the backend and holds months, weekdays, am/pm, ad/bc,
   the RESERV words (`now`, `today`, `epoch`, `infinity`, `allballs`) and
   the ISO unit letters. The ZONE ABBREVIATIONS are not in it --
   datetime.c:100-103 says so outright: \"The static table contains no TZ,
   DTZ, or DYNTZ entries; rather those are loaded from configuration
   files\" -- they come from `src/timezone/tznames/Default`, and
   `DecodeTimezoneAbbrev` is consulted BEFORE `DecodeSpecial`
   (datetime.c:1304-1309).

   That order is not negotiable, and the comment at datetime.c:3203-3208
   says why: tzdb deliberately contains zone NAMES identical to offset
   ABBREVIATIONS. With the `Default` set the two token sets happen not to
   collide at all (checked below), so the precedence is currently a
   no-op -- it stops being one the moment `timezone_abbreviations` is
   settable, which is why it is written down rather than relied upon.

   An abbreviation is a FIXED OFFSET; a zone NAME is a RULE. `pst` here
   is -28800 seconds, always, with no DST -- while `PST8PDT` is a tzdb
   LINK to America/Los_Angeles and does observe DST. Resolving an
   abbreviation through `ZoneId/of` with `SHORT_IDS` (which maps PST to
   America/Los_Angeles) gets every July timestamp wrong by an hour, with
   no error, which is what this table exists to stop.

   Keys are LOWERCASE. `ParseDateTime` lowercases every alpha run with
   `pg_tolower`, which is ASCII-only (pgstrcasecmp.c:122-129), and
   `tzparser.c:79-83` lowercases every abbreviation with the comment
   \"must match datetime.c's conversion\". So a lookup must fold ASCII
   only -- never `clojure.string/lower-case`, which is locale-sensitive
   and breaks under a Turkish default locale.

   GENERATED -- the extraction commands are in the comment below."
  (:require [clojure.string :as str]))

;; Regenerating after a PostgreSQL bump. From .internal/postgres-REL_<ref>:
;;
;;   sed -n '/^static const datetkn datetktbl\[\] = {/,/^};/p' \
;;     src/backend/utils/adt/datetime.c \
;;   | grep -oE '^[[:space:]]*\{[^}]*\}' \
;;   | sed 's/^[[:space:]]*{//; s/}$//; s/"//g; s/,[[:space:]]*/\t/g' \
;;   | awk -F'\t' 'BEGIN{m["EARLY"]="-infinity"; m["LATE"]="infinity";
;;       m["DA_D"]="ad"; m["DB_C"]="bc"; m["EPOCH"]="epoch"; m["NOW"]="now";
;;       m["TODAY"]="today"; m["TOMORROW"]="tomorrow";
;;       m["YESTERDAY"]="yesterday"}
;;     {t=$1; if (t in m) t=m[t];
;;      v=$3; if (v=="SECS_PER_HOUR") v=3600;
;;      print t "\t" $2 "\t" v}'
;;
;;   awk '/^[A-Za-z]/ { a=tolower($1); v=$2;
;;     if (v ~ /^-?[0-9]+$/) printf "%s\t%s\t%s\n", a, v, (($3=="D")?"DTZ":"TZ");
;;     else                  printf "%s\t%s\tDYNTZ\n", a, v }' \
;;     src/timezone/tznames/Default
;;
;; The nine tokens whose C spelling is a macro (EARLY, LATE, DA_D, DB_C,
;; EPOCH, NOW, TODAY, TOMORROW, YESTERDAY -- datetime.h:35-62) are
;; resolved by the awk; without that they would transcribe as the macro
;; NAMES and every one of those words would stop parsing.

(defn ascii-lower
  "`pg_tolower`'s fold: A-Z only. `clojure.string/lower-case` is
   locale-sensitive -- under a Turkish default locale it maps `I` to a
   dotless i and every abbreviation starting with I stops resolving."
  ^String [^String s]
  (let [n (.length s)
        sb (StringBuilder. n)]
    (dotimes [i n]
      (let [c (.charAt s i)]
        (.append sb (if (and (>= (int c) (int \A)) (<= (int c) (int \Z)))
                      (char (+ (int c) 32))
                      c))))
    (.toString sb)))

(def datetktbl
  "`datetktbl` (datetime.c:105-179), 72 entries: {token -> [type value]}.

   The `type` and `value` are kept as KEYWORDS rather than resolved to
   the C integers. The numbers only exist because C needed bitmask
   arithmetic over them; what the decoder actually wants is a tag and a
   set, and a keyword says which rule applies without a lookup table of
   its own. A numeric `value` (a month number, a weekday number, an
   offset in seconds) stays a number."
  {"+infinity"   [:reserv      :late]
   "-infinity"   [:reserv      :early]
   "ad"          [:adbc        :ad]
   "allballs"    [:reserv      :zulu]
   "am"          [:ampm        :am]
   "apr"         [:month       4]
   "april"       [:month       4]
   "at"          [:ignore-dtf  0]
   "aug"         [:month       8]
   "august"      [:month       8]
   "bc"          [:adbc        :bc]
   "d"           [:units       :day]
   "dec"         [:month       12]
   "december"    [:month       12]
   "dow"         [:units       :dow]
   "doy"         [:units       :doy]
   ;; SECS_PER_HOUR. The ONLY numeric macro among the values -- the
   ;; rest are DTK_* labels, which stay keywords on purpose.
   "dst"         [:dtzmod      3600]
   "epoch"       [:reserv      :epoch]
   "feb"         [:month       2]
   "february"    [:month       2]
   "fri"         [:dow         5]
   "friday"      [:dow         5]
   "h"           [:units       :hour]
   "infinity"    [:reserv      :late]
   "isodow"      [:units       :isodow]
   "isoyear"     [:units       :isoyear]
   "j"           [:units       :julian]
   "jan"         [:month       1]
   "january"     [:month       1]
   "jd"          [:units       :julian]
   "jul"         [:month       7]
   "julian"      [:units       :julian]
   "july"        [:month       7]
   "jun"         [:month       6]
   "june"        [:month       6]
   "m"           [:units       :month]
   "mar"         [:month       3]
   "march"       [:month       3]
   "may"         [:month       5]
   "mm"          [:units       :minute]
   "mon"         [:dow         1]
   "monday"      [:dow         1]
   "nov"         [:month       11]
   "november"    [:month       11]
   "now"         [:reserv      :now]
   "oct"         [:month       10]
   "october"     [:month       10]
   "on"          [:ignore-dtf  0]
   "pm"          [:ampm        :pm]
   "s"           [:units       :second]
   "sat"         [:dow         6]
   "saturday"    [:dow         6]
   "sep"         [:month       9]
   "sept"        [:month       9]
   "september"   [:month       9]
   "sun"         [:dow         0]
   "sunday"      [:dow         0]
   "t"           [:isotime     :time]
   "thu"         [:dow         4]
   "thur"        [:dow         4]
   "thurs"       [:dow         4]
   "thursday"    [:dow         4]
   "today"       [:reserv      :today]
   "tomorrow"    [:reserv      :tomorrow]
   "tue"         [:dow         2]
   "tues"        [:dow         2]
   "tuesday"     [:dow         2]
   "wed"         [:dow         3]
   "wednesday"   [:dow         3]
   "weds"        [:dow         3]
   "y"           [:units       :year]
   "yesterday"   [:reserv      :yesterday]})

(def zone-abbrevs
  "`src/timezone/tznames/Default`, 195 entries: 97 TZ, 48 DTZ, 50 DYNTZ.

   TZ   a fixed offset, standard time: {:type :tz :offset <seconds east>}
   DTZ  a fixed offset, daylight time; sets tm_isdst
        (datetime.c:1406-1418)
   DYNTZ  resolved against a zone at the instant in question
        (`DetermineTimeZoneAbbrevOffset`): {:type :dyntz :zone \"...\"}

   `ConvertTimeZoneAbbrevs` (datetime.c:4873-4951) assigns DYNTZ when the
   second column is a zone name and `is_dst ? DTZ : TZ` otherwise.

   The offsets here are SECONDS EAST, as the file writes them. PostgreSQL
   stores `tzp` internally as seconds WEST and negates on the way in
   (`*tzp = -tz`, datetime.c:3083) -- getting that backwards is a silent
   double-offset error, so the sign convention is stated at every
   boundary."
  {"eat"     {:type :tz   :offset 10800}
   "sast"    {:type :tz   :offset 7200}
   "wat"     {:type :tz   :offset 3600}
   "act"     {:type :tz   :offset -18000}
   "akdt"    {:type :dtz  :offset -28800}
   "akst"    {:type :tz   :offset -32400}
   "art"     {:type :dyntz :zone "America/Argentina/Buenos_Aires"}
   "arst"    {:type :dyntz :zone "America/Argentina/Buenos_Aires"}
   "bot"     {:type :tz   :offset -14400}
   "bra"     {:type :tz   :offset -10800}
   "brst"    {:type :dtz  :offset -7200}
   "brt"     {:type :tz   :offset -10800}
   "cot"     {:type :tz   :offset -18000}
   "cdt"     {:type :dtz  :offset -18000}
   "clst"    {:type :dtz  :offset -10800}
   "clt"     {:type :dyntz :zone "America/Santiago"}
   "cst"     {:type :tz   :offset -21600}
   "edt"     {:type :dtz  :offset -14400}
   "egst"    {:type :dtz  :offset 0}
   "egt"     {:type :tz   :offset -3600}
   "est"     {:type :tz   :offset -18000}
   "fnt"     {:type :tz   :offset -7200}
   "fnst"    {:type :dtz  :offset -3600}
   "gft"     {:type :tz   :offset -10800}
   "gyt"     {:type :dyntz :zone "America/Guyana"}
   "mdt"     {:type :dtz  :offset -21600}
   "mst"     {:type :tz   :offset -25200}
   "ndt"     {:type :dtz  :offset -9000}
   "nft"     {:type :tz   :offset -12600}
   "nst"     {:type :tz   :offset -12600}
   "pet"     {:type :tz   :offset -18000}
   "pdt"     {:type :dtz  :offset -25200}
   "pmdt"    {:type :dtz  :offset -7200}
   "pmst"    {:type :tz   :offset -10800}
   "pst"     {:type :tz   :offset -28800}
   "pyst"    {:type :dtz  :offset -10800}
   "pyt"     {:type :dyntz :zone "America/Asuncion"}
   "uyst"    {:type :dtz  :offset -7200}
   "uyt"     {:type :tz   :offset -10800}
   "vet"     {:type :dyntz :zone "America/Caracas"}
   "wgst"    {:type :dtz  :offset -7200}
   "wgt"     {:type :tz   :offset -10800}
   "davt"    {:type :dyntz :zone "Antarctica/Davis"}
   "ddut"    {:type :tz   :offset 36000}
   "mawt"    {:type :dyntz :zone "Antarctica/Mawson"}
   "aft"     {:type :tz   :offset 16200}
   "almt"    {:type :tz   :offset 21600}
   "almst"   {:type :dtz  :offset 25200}
   "amst"    {:type :dyntz :zone "Asia/Yerevan"}
   "amt"     {:type :tz   :offset -14400}
   "anast"   {:type :dyntz :zone "Asia/Anadyr"}
   "anat"    {:type :dyntz :zone "Asia/Anadyr"}
   "azst"    {:type :dyntz :zone "Asia/Baku"}
   "azt"     {:type :dyntz :zone "Asia/Baku"}
   "bdt"     {:type :tz   :offset 21600}
   "bnt"     {:type :tz   :offset 28800}
   "bort"    {:type :tz   :offset 28800}
   "btt"     {:type :tz   :offset 21600}
   "cct"     {:type :tz   :offset 28800}
   "gest"    {:type :dyntz :zone "Asia/Tbilisi"}
   "get"     {:type :dyntz :zone "Asia/Tbilisi"}
   "hkt"     {:type :tz   :offset 28800}
   "ict"     {:type :tz   :offset 25200}
   "idt"     {:type :dtz  :offset 10800}
   "irkst"   {:type :dyntz :zone "Asia/Irkutsk"}
   "irkt"    {:type :dyntz :zone "Asia/Irkutsk"}
   "irt"     {:type :tz   :offset 12600}
   "ist"     {:type :tz   :offset 7200}
   "jayt"    {:type :tz   :offset 32400}
   "jst"     {:type :tz   :offset 32400}
   "kdt"     {:type :dtz  :offset 36000}
   "kgst"    {:type :dtz  :offset 21600}
   "kgt"     {:type :dyntz :zone "Asia/Bishkek"}
   "krast"   {:type :dyntz :zone "Asia/Krasnoyarsk"}
   "krat"    {:type :dyntz :zone "Asia/Krasnoyarsk"}
   "kst"     {:type :tz   :offset 32400}
   "lkt"     {:type :dyntz :zone "Asia/Colombo"}
   "magst"   {:type :dyntz :zone "Asia/Magadan"}
   "magt"    {:type :dyntz :zone "Asia/Magadan"}
   "mmt"     {:type :tz   :offset 23400}
   "myt"     {:type :tz   :offset 28800}
   "novst"   {:type :dyntz :zone "Asia/Novosibirsk"}
   "novt"    {:type :dyntz :zone "Asia/Novosibirsk"}
   "npt"     {:type :tz   :offset 20700}
   "omsst"   {:type :dyntz :zone "Asia/Omsk"}
   "omst"    {:type :dyntz :zone "Asia/Omsk"}
   "petst"   {:type :dyntz :zone "Asia/Kamchatka"}
   "pett"    {:type :dyntz :zone "Asia/Kamchatka"}
   "pht"     {:type :tz   :offset 28800}
   "pkt"     {:type :tz   :offset 18000}
   "pkst"    {:type :dtz  :offset 21600}
   "sgt"     {:type :dyntz :zone "Asia/Singapore"}
   "tjt"     {:type :tz   :offset 18000}
   "tmt"     {:type :dyntz :zone "Asia/Ashgabat"}
   "ulast"   {:type :dtz  :offset 32400}
   "ulat"    {:type :dyntz :zone "Asia/Ulaanbaatar"}
   "uzst"    {:type :dtz  :offset 21600}
   "uzt"     {:type :tz   :offset 18000}
   "vlast"   {:type :dyntz :zone "Asia/Vladivostok"}
   "vlat"    {:type :dyntz :zone "Asia/Vladivostok"}
   "xjt"     {:type :tz   :offset 21600}
   "yakst"   {:type :dyntz :zone "Asia/Yakutsk"}
   "yakt"    {:type :dyntz :zone "Asia/Yakutsk"}
   "yekst"   {:type :dtz  :offset 21600}
   "yekt"    {:type :dyntz :zone "Asia/Yekaterinburg"}
   "adt"     {:type :dtz  :offset -10800}
   "ast"     {:type :tz   :offset -14400}
   "azost"   {:type :dtz  :offset 0}
   "azot"    {:type :tz   :offset -3600}
   "fkst"    {:type :dyntz :zone "Atlantic/Stanley"}
   "fkt"     {:type :dyntz :zone "Atlantic/Stanley"}
   "acsst"   {:type :dtz  :offset 37800}
   "acdt"    {:type :dtz  :offset 37800}
   "acst"    {:type :tz   :offset 34200}
   "acwst"   {:type :tz   :offset 31500}
   "aesst"   {:type :dtz  :offset 39600}
   "aedt"    {:type :dtz  :offset 39600}
   "aest"    {:type :tz   :offset 36000}
   "awsst"   {:type :dtz  :offset 32400}
   "awst"    {:type :tz   :offset 28800}
   "cadt"    {:type :dtz  :offset 37800}
   "cast"    {:type :tz   :offset 34200}
   "lhdt"    {:type :dyntz :zone "Australia/Lord_Howe"}
   "lhst"    {:type :tz   :offset 37800}
   "ligt"    {:type :tz   :offset 36000}
   "nzt"     {:type :tz   :offset 43200}
   "sadt"    {:type :dtz  :offset 37800}
   "wadt"    {:type :dtz  :offset 28800}
   "wast"    {:type :tz   :offset 25200}
   "wdt"     {:type :dtz  :offset 32400}
   "gmt"     {:type :tz   :offset 0}
   "uct"     {:type :tz   :offset 0}
   "ut"      {:type :tz   :offset 0}
   "utc"     {:type :tz   :offset 0}
   "z"       {:type :tz   :offset 0}
   "zulu"    {:type :tz   :offset 0}
   "bst"     {:type :dtz  :offset 3600}
   "bdst"    {:type :dtz  :offset 7200}
   "cest"    {:type :dtz  :offset 7200}
   "cet"     {:type :tz   :offset 3600}
   "cetdst"  {:type :dtz  :offset 7200}
   "eest"    {:type :dtz  :offset 10800}
   "eet"     {:type :tz   :offset 7200}
   "eetdst"  {:type :dtz  :offset 10800}
   "fet"     {:type :tz   :offset 10800}
   "mest"    {:type :dtz  :offset 7200}
   "mesz"    {:type :dtz  :offset 7200}
   "met"     {:type :tz   :offset 3600}
   "metdst"  {:type :dtz  :offset 7200}
   "mez"     {:type :tz   :offset 3600}
   "msd"     {:type :dtz  :offset 14400}
   "msk"     {:type :dyntz :zone "Europe/Moscow"}
   "volt"    {:type :dyntz :zone "Europe/Volgograd"}
   "wet"     {:type :tz   :offset 0}
   "wetdst"  {:type :dtz  :offset 3600}
   "cxt"     {:type :tz   :offset 25200}
   "iot"     {:type :dyntz :zone "Indian/Chagos"}
   "mut"     {:type :tz   :offset 14400}
   "must"    {:type :dtz  :offset 18000}
   "mvt"     {:type :tz   :offset 18000}
   "ret"     {:type :tz   :offset 14400}
   "sct"     {:type :tz   :offset 14400}
   "tft"     {:type :tz   :offset 18000}
   "chadt"   {:type :dtz  :offset 49500}
   "chast"   {:type :tz   :offset 45900}
   "chut"    {:type :tz   :offset 36000}
   "ckt"     {:type :dyntz :zone "Pacific/Rarotonga"}
   "easst"   {:type :dyntz :zone "Pacific/Easter"}
   "east"    {:type :dyntz :zone "Pacific/Easter"}
   "fjst"    {:type :dtz  :offset 46800}
   "fjt"     {:type :tz   :offset 43200}
   "galt"    {:type :tz   :offset -21600}
   "gamt"    {:type :tz   :offset -32400}
   "gilt"    {:type :tz   :offset 43200}
   "hst"     {:type :tz   :offset -36000}
   "kost"    {:type :dyntz :zone "Pacific/Kosrae"}
   "lint"    {:type :dyntz :zone "Pacific/Kiritimati"}
   "mart"    {:type :tz   :offset -34200}
   "mht"     {:type :tz   :offset 43200}
   "mpt"     {:type :tz   :offset 36000}
   "nut"     {:type :dyntz :zone "Pacific/Niue"}
   "nzdt"    {:type :dtz  :offset 46800}
   "nzst"    {:type :tz   :offset 43200}
   "pgt"     {:type :tz   :offset 36000}
   "pont"    {:type :tz   :offset 39600}
   "pwt"     {:type :tz   :offset 32400}
   "taht"    {:type :tz   :offset -36000}
   "tkt"     {:type :dyntz :zone "Pacific/Fakaofo"}
   "tot"     {:type :tz   :offset 46800}
   "trut"    {:type :tz   :offset 36000}
   "tvt"     {:type :tz   :offset 43200}
   "vut"     {:type :tz   :offset 39600}
   "wakt"    {:type :tz   :offset 43200}
   "wft"     {:type :tz   :offset 43200}
   "yapt"    {:type :tz   :offset 36000}})

(defn special-token
  "`DecodeSpecial` (datetime.c:3148-3188): look `s` up in `datetktbl`.
   Returns `[type value]` or nil. `s` must already be ASCII-lowercased,
   as `ParseDateTime` leaves it."
  [s]
  (get datetktbl s))

(defn zone-abbrev
  "`DecodeTimezoneAbbrev` (datetime.c:3091-3146): look `s` up in the
   abbreviation table ONLY -- never in `datetktbl`, and never through
   `pg_tzset`. Returns the entry or nil; nil is `UNKNOWN_FIELD`, which is
   what sends the caller on to `DecodeSpecial` and then to `pg_tzset`."
  [s]
  (get zone-abbrevs s))

;; PostgreSQL's `datebsearch` (datetime.c:4153-4187) compares with
;; `strncmp(key, token, TOKMAXLEN)` where TOKMAXLEN is 10 -- so it is a
;; PREFIX match at ten characters, which is how `millisecond` and
;; `milliseconds` both resolve to the one `"millisecon"` entry. A plain
;; map lookup is NOT equivalent in general.
;;
;; It is equivalent for these two tables, because no key in either
;; reaches ten characters: `datetktbl`'s longest is 9 and `tzparser.c`
;; rejects any abbreviation over 10 (tzparser.c:59-65). That is an
;; invariant of the DATA, not of the code, so it is asserted rather than
;; assumed -- see `pg_datetime_tokens_test`.
(def ^:const tokmaxlen 10)

(defn prefix-safe?
  "Is a plain map lookup equivalent to `datebsearch`'s 10-character
   prefix match for this table? True when no key reaches `tokmaxlen`."
  [table]
  (every? #(< (count %) tokmaxlen) (keys table)))
