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

   SIGN: everything here returns and accepts SECONDS WEST, as
   PostgreSQL stores internally."
  (:require [datahike.pg.datetime.decode :as dec])
  (:import [java.time LocalDateTime ZoneId ZoneOffset]
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
  (let [folded (delay (into {} (map (juxt #(.toLowerCase ^String %) identity))
                            (ZoneId/getAvailableZoneIds)))]
    (or (try (ZoneId/of name) (catch ZoneRulesException _ nil)
             (catch java.time.DateTimeException _ nil))
        (when-let [id (get @folded (.toLowerCase name))]
          (ZoneId/of id)))))

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

(defn determine-offset
  "`DetermineTimeZoneOffsetInternal`. The local fields and a zone to
   SECONDS WEST.

   Three cases, and the last two are where this differs from
   `ZonedDateTime/of`:

     one valid offset   use it
     none (a gap)       the BEFORE offset, local fields unchanged
     two (an overlap)   the AFTER offset

   An out-of-range date is not an error: the C falls through to
   `overflow` and assumes UTC (datetime.c:1726-1730)."
  ^long [{:keys [year mon mday hour min sec]} ^ZoneId z]
  (let [rules (.getRules z)]
    (try
      ;; The C builds the local time as if it were GMT and then probes.
      ;; `hour` may be 24 and `sec` may be 60, neither of which
      ;; LocalDateTime accepts, so the overflow is carried in rather
      ;; than clamped -- the offset for 24:00 is the offset for the
      ;; following midnight, which is what the C's seconds arithmetic
      ;; computes.
      (let [base (LocalDateTime/of (int year) (int mon) (int mday) 0 0 0)
            ldt (-> base
                    (.plusHours (long hour))
                    (.plusMinutes (long min))
                    (.plusSeconds (long sec)))
            valid (.getValidOffsets rules ldt)]
        (cond
          (= 1 (.size valid))
          (- (.getTotalSeconds ^ZoneOffset (.get valid 0)))

          ;; A GAP. PostgreSQL prefers the BEFORE reading
          ;; (datetime.c:1703-1708); java.time would shift forward and
          ;; use the after offset.
          (.isEmpty valid)
          (- (.getTotalSeconds (.getOffsetBefore (.getTransition rules ldt))))

          ;; An OVERLAP. PostgreSQL prefers the AFTER reading;
          ;; `ZonedDateTime/of` would take the first, which is before.
          :else
          (- (.getTotalSeconds ^ZoneOffset (.get valid 1)))))
      (catch java.time.DateTimeException _ 0))))

(defn determine-abbrev-offset
  "`DetermineTimeZoneAbbrevOffset` (datetime.c:1748-1779) for a DYNTZ
   abbreviation -- one that names a zone instead of carrying an offset.

   PARTIAL, and deliberately so. The C first asks the zone's own
   transition data whether the abbreviation as WRITTEN matches at that
   instant, and uses that offset if it does; only on a miss does it
   fall back to the zone's offset. Java exposes no abbreviation strings
   from tzdb, so only the fallback is implementable here without
   shipping the tzdb text ourselves.

   What that costs: writing an abbreviation that disagrees with the
   zone's state at the instant -- `'2000-01-01 12:00 ARST'`, summer
   time in Buenos Aires named on a winter date -- gets the zone's
   offset for that date rather than the abbreviation's own. The
   abbreviation still selects the right ZONE, so it is wrong only when
   the two disagree, and only for the 50 DYNTZ entries of 195. Recorded
   in doc/review-backlog.md rather than papered over."
  ^long [tm ^String zone-name]
  (if-let [z (resolve-zone-name zone-name)]
    (determine-offset tm z)
    0))
