(ns datahike.pg.datetime.carrier
  "The port's canonical values in the carriers the rest of pg-datahike
   already uses.

   The parser returns PostgreSQL's own internal form -- a day count, or
   microseconds from an epoch. Everything downstream expects
   `java.time.LocalDate`, `LocalTime`, `OffsetTime`, `LocalDateTime` or
   `java.util.Date`, with two established conventions for the values
   those classes cannot hold. This namespace is the one place the two
   meet, and it is deliberately thin: the conversion is where precision
   is lost, so it should be visible rather than spread across call
   sites.

   THREE THINGS THE CARRIERS CANNOT HOLD, each with a convention that
   predates this port:

   `24:00:00` is a legal PostgreSQL time and `java.time.LocalTime` has
   no end-of-day value. It is carried as the TEXT `\"24:00:00\"`, which
   stores, renders and orders correctly because time values are kept
   as their canonical text.

   The two infinities have no `java.util.Date` room, so they are the
   sentinels `types/pos-infinity` and `types/neg-infinity`.

   MICROSECONDS survive `LocalDateTime`, `LocalTime` and
   `OffsetDateTime`, and do NOT survive `java.util.Date`, which is
   millisecond-only. The parser is not the lossy step and never was --
   this conversion is, and only on the paths that must hand back a
   `Date`."
  (:require [datahike.pg.datetime.in :as in]
            [datahike.pg.types :as types])
  (:import [java.time LocalDate LocalDateTime LocalTime OffsetTime ZoneOffset]))

(def ^:const end-of-day-usec 86400000000)

(defn- offset-of
  "SECONDS WEST to a `ZoneOffset`, which counts EAST."
  ^ZoneOffset [west]
  (ZoneOffset/ofTotalSeconds (int (- (long west)))))

(defn ->date
  "`:date` to `LocalDate`, or an infinity sentinel."
  [r]
  (case (:kind r)
    :infinity types/pos-infinity
    :-infinity types/neg-infinity
    (let [{:keys [year mon mday]} (in/->fields r)]
      (LocalDate/of (int year) (int mon) (int mday)))))

(defn ->time
  "`:time` to `LocalTime`, or the text `\"24:00:00\"` for end of day."
  [r]
  (if (= (:usec r) end-of-day-usec)
    "24:00:00"
    (LocalTime/ofNanoOfDay (* (long (:usec r)) 1000))))

(defn ->timetz
  "`:timetz` to `OffsetTime`, or the end-of-day text with its offset."
  [r]
  (let [off (offset-of (:west r))]
    (if (= (:usec r) end-of-day-usec)
      (str "24:00:00" (types/offset-text off))
      (OffsetTime/of (LocalTime/ofNanoOfDay (* (long (:usec r)) 1000)) off))))

(defn ->local-datetime
  "`:timestamp`/`:timestamptz` to `LocalDateTime`, which keeps
   microseconds, or an infinity sentinel."
  [r]
  (case (:kind r)
    :infinity types/pos-infinity
    :-infinity types/neg-infinity
    (let [{:keys [year mon mday hour min sec usec]} (in/->fields r)]
      (LocalDateTime/of (int year) (int mon) (int mday)
                        (int hour) (int min) (int sec) (int (* usec 1000))))))

(defn ->offset-datetime
  "`:timestamptz` to `OffsetDateTime` at +00.

   The microsecond-preserving carrier for a timestamptz. `types/->pg-text`
   already renders one with its offset, so this keeps BOTH the `+00`
   that a timestamptz must print and the precision a `java.util.Date`
   drops -- which were previously a choice of one or the other."
  [r]
  (case (:kind r)
    :infinity types/pos-infinity
    :-infinity types/neg-infinity
    (.atOffset ^LocalDateTime (->local-datetime r) ZoneOffset/UTC)))

(defn ->date-value
  "`:timestamp`/`:timestamptz` to `java.util.Date`.

   THIS IS THE LOSSY ONE. `java.util.Date` is millisecond-only, so
   `.123456` becomes `.123`. Used where a store or a wire path still
   expects a Date; prefer `->local-datetime` wherever the caller can
   take it."
  [r]
  (case (:kind r)
    :infinity types/pos-infinity
    :-infinity types/neg-infinity
    (java.util.Date/from (.toInstant ^LocalDateTime (->local-datetime r)
                                     ZoneOffset/UTC))))
