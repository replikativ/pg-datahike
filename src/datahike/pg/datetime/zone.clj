(ns datahike.pg.datetime.zone
  "`DetermineTimeZoneOffset` (datetime.c:1575-1733): a local date and
   time plus a zone to an OFFSET.

   This is a separate step from parsing because an offset is a function
   of the INSTANT, and the instant is only known once the date has been
   decoded and validated.

   THE AMBIGUITY RULE IS THE OPPOSITE OF `java.time`'s, in both
   directions, and this is the whole reason the namespace exists rather
   than a call to `ZonedDateTime/of`:

     spring forward (a GAP, the local time never happened)
       PostgreSQL  takes the BEFORE offset and keeps the local fields
       java.time   shifts the time forward and takes the AFTER offset

     fall back (an OVERLAP, the local time happened twice)
       PostgreSQL  takes the AFTER offset
       java.time   takes the earlier, BEFORE offset

   datetime.c:1697-1706 states the rule and why it is not phrased as
   \"prefer standard time\": that older rule could not resolve zones
   where both readings report as standard time -- Europe/Moscow in
   October 2014 -- and in zones like Europe/Dublin there is widespread
   disagreement about which offset is \"standard\" at all.

   An hour wrong, twice a year, with no error, is exactly the failure
   this port is meant to remove, so it is implemented from the C's
   branches rather than from the convenience method.

   The input decoders use integer seconds west of UTC, as PostgreSQL
   does internally. ZoneOffset values use java.time's usual sign."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [datahike.pg.datetime.tokens :as tokens])
  (:import [java.time Instant LocalDateTime ZoneId ZoneOffset]
           [java.time.zone ZoneRules ZoneRulesException]))

(def ^:private posix-tz
  "A POSIX TZ string of the simplest shape: a name, then an offset.
   `GMT+8`, `UTC-5`, `PST+8`."
  #"([A-Za-z]{3,})([+-]?)(\d{1,2})(?::(\d{2}))?(?::(\d{2}))?")

(defn posix-offset
  "`GMT+8` and friends, as SECONDS WEST -- or nil if `name` is not one.

   THE SIGN IS INVERTED relative to everything else, and this is not a
   quirk of ours: in a POSIX TZ string the offset is \"the value added
   to local time to arrive at UTC\", so `GMT+8` is eight hours BEHIND
   UTC. The oracle agrees -- `'2000-01-01 12:00:00 GMT+8'::timestamptz`
   is `20:00:00+00`.

   Java reads it the other way. `ZoneId/of(\"GMT+8\")` is +08:00, so
   routing these through `ZoneId` gives an answer sixteen hours out
   with no error. That is why this is checked BEFORE the tzdb lookup
   rather than after.

   It does not shadow real zone names: `PST8PDT` and `EST5EDT` have a
   second alpha run and do not match, and `Etc/GMT+5` has a `/`. Those
   go to tzdb, which applies the same inversion itself."
  [^String name]
  (when-let [[_ _ sign h m sec] (re-matches posix-tz name)]
    (let [v (+ (* (Long/parseLong h) 3600)
               (* (if m (Long/parseLong m) 0) 60)
               (if sec (Long/parseLong sec) 0))]
      (if (= sign "-") (- v) v))))

(defn posix-zone-seconds-west
  "Decode a fixed POSIX zone specification using PostgreSQL's getoffset
   bounds. The seconds field admits 60. The abbreviation may be empty,
   quoted, or attached directly to the offset."
  [name]
  (when-let [[_ sign hour minute second]
             (re-matches #"(?:<[^>]*>|(?![<:])[^0-9,+-]*)([+-]?)([0-9]+)(?::([0-9]+)(?::([0-9]+))?)?"
                         name)]
    (let [hour (parse-long hour)
          minute (parse-long (or minute "0"))
          second (parse-long (or second "0"))]
      (when (and hour minute second (<= hour 167) (<= minute 59) (<= second 60))
        (let [west (+ (* 3600 hour) (* 60 minute) second)]
          (if (= sign "-") (- west) west))))))

(def ^:private names-by-lowercase
  (delay (into {} (map (juxt #(.toLowerCase ^String % java.util.Locale/ROOT) identity))
               (ZoneId/getAvailableZoneIds))))

(defn registered-zone-id
  "Look up a tzdb ID without accepting Java's synthetic GMT+ or UTC+ IDs."
  ^ZoneId [^String name]
  (when-let [id (get @names-by-lowercase (.toLowerCase name java.util.Locale/ROOT))]
    (ZoneId/of id)))

(defn zone-id
  "`pg_tzset`: a zone NAME to a zone, or nil if there is no such zone.

   Case-insensitive, because `pg_tzset` is and because the lexer has
   already lowercased the field. Java's own ids are mixed case, so the
   lookup is against a folded index.

   `ZoneId/of` with `SHORT_IDS` is deliberately NOT used: it maps `PST`
   to America/Los_Angeles, which observes DST, while PostgreSQL's `PST`
   is a fixed -08:00 from the abbreviation table. Abbreviations never
   reach this function -- they are resolved earlier, against
   `tokens/zone-abbrevs` -- and routing them here would be the bug."
  [^String name]
  (or (try (ZoneId/of name) (catch ZoneRulesException _ nil)
           (catch java.time.DateTimeException _ nil))
      (registered-zone-id name)))

(def utc
  "The session zone until `SET TimeZone` is honoured (expr.clj:1613
   reports UTC unconditionally)."
  (ZoneId/of "UTC"))

(def ^:private zone-cache (atom {}))

(defn resolve-zone-name
  "Cached `zone-id`. Zone lookup walks every available id on a miss and
   a literal can carry any string, so the misses are cached too."
  [^String name]
  (let [c @zone-cache]
    (if (contains? c name)
      (get c name)
      (let [z (zone-id name)]
        (swap! zone-cache assoc name z)
        z))))

(defn fixed-offset
  "`pg_get_timezone_offset`: the zone's offset if it has exactly one for
   all time, else nil. `DecodeTimeOnly` needs this -- a time with no
   date can take a zone only when the answer does not depend on the
   date, so `'12:00 UTC'::timetz` works and
   `'12:00 America/New_York'::timetz` does not."
  [^ZoneId z]
  (let [rules (.getRules z)]
    (when (.isFixedOffset rules)
      (- (.getTotalSeconds (.getOffset rules java.time.Instant/EPOCH))))))

(defn dynamic-abbreviation [^String name]
  (let [abbrev (.toLowerCase (.trim name) java.util.Locale/ROOT)
        entry (tokens/zone-abbrev abbrev)]
    (when (= :dyntz (:type entry))
      (assoc entry :abbrev abbrev))))

(def ^:private abbreviation-offsets
  (delay (edn/read-string (slurp (io/resource "datahike/pg/datetime/abbreviation_offsets.edn")))))

(defn- historical-offset [abbrev second]
  (when-let [transitions (get @abbreviation-offsets abbrev)]
    ;; Each entry supersedes the previous occurrence of this abbreviation.
    ;; The first entry also supplies its meaning before it first appeared.
    (some (fn [[start east]]
            (when (<= start second) (ZoneOffset/ofTotalSeconds (int east))))
          (rseq transitions))))

(defn offset-at-local-time
  "Choose the zone offset for a local time using PostgreSQL's transition rules.
   A gap uses the offset before the transition. An overlap uses the offset
   after the transition."
  (^ZoneOffset [^LocalDateTime ldt ^ZoneId zone]
   (let [rules (.getRules zone)
         valid (.getValidOffsets rules ldt)]
     (if (.isEmpty valid)
       (.getOffsetBefore (.getTransition rules ldt))
       (.get valid (dec (.size valid))))))
  (^ZoneOffset [^LocalDateTime ldt ^ZoneId zone abbrev]
   (let [offset (offset-at-local-time ldt zone)]
     (or (when abbrev (historical-offset abbrev (.toEpochSecond ldt offset))) offset))))

(defn offset-at-instant
  "Resolve a dynamic abbreviation at an instant, falling back to its zone
   when the abbreviation does not appear in the transition data."
  ^ZoneOffset [^Instant instant ^ZoneId zone abbrev]
  (or (when abbrev (historical-offset abbrev (.getEpochSecond instant)))
      (.getOffset (.getRules zone) instant)))

(defn- wall-clock ^LocalDateTime [{:keys [year mon mday hour min sec]}]
  ;; Carry 24:00 or a leap second into the following day or minute.
  (-> (LocalDateTime/of (int year) (int mon) (int mday) 0 0 0)
      (.plusHours (long hour))
      (.plusMinutes (long min))
      (.plusSeconds (long sec))))

(defn determine-offset
  "Convert local fields and a zone to seconds west of UTC.
   An out-of-range date falls back to UTC (datetime.c:1726-1730)."
  ^long [tm ^ZoneId zone]
  (try
    (- (.getTotalSeconds (offset-at-local-time (wall-clock tm) zone)))
    (catch java.time.DateTimeException _ 0)))

(defn determine-abbrev-offset
  "`DetermineTimeZoneAbbrevOffset` in seconds west of UTC."
  ^long [tm ^String zone-name abbrev]
  (if-let [z (resolve-zone-name zone-name)]
    (try
      (- (.getTotalSeconds (offset-at-local-time (wall-clock tm) z abbrev)))
      (catch java.time.DateTimeException _ 0))
    0))
