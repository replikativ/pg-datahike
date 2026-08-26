(ns datahike.pg.sql.set-ops
  "Value coercion shared by top-level and nested set-operation executors."
  (:require [datahike.pg.arrays :as pg-arr]
            [clojure.string :as str]
            [datahike.pg.errors :as errors]
            [datahike.pg.sql.cast :as sql-cast]
            [datahike.pg.types :as types]))

(defn- invalid-timestamptz! [v]
  (throw (errors/pg-error :invalid-text-representation
                          {:type "timestamp with time zone"
                           :value v})))

(defn- timestamptz-date [v]
  (cond
    (instance? java.util.Date v) v
    (instance? java.time.Instant v) (java.util.Date/from ^java.time.Instant v)
    (instance? java.time.OffsetDateTime v)
    (java.util.Date/from (.toInstant ^java.time.OffsetDateTime v))
    (instance? java.time.ZonedDateTime v)
    (java.util.Date/from (.toInstant ^java.time.ZonedDateTime v))
    (instance? java.time.LocalDateTime v)
    (java.util.Date/from
     (.toInstant ^java.time.LocalDateTime v java.time.ZoneOffset/UTC))
    (instance? java.time.LocalDate v)
    (java.util.Date/from
     (.toInstant (.atStartOfDay ^java.time.LocalDate v) java.time.ZoneOffset/UTC))
    (string? v)
    (let [s (-> v str/trim
                (str/replace #"(\d{4}-\d{2}-\d{2})\s+(\d)" "$1T$2")
                (str/replace #"([+-]\d{2})(\d{2})$" "$1:$2")
                (str/replace #"([+-]\d{2})$" "$1:00"))]
      (or (try (java.util.Date/from (java.time.Instant/parse s))
               (catch Exception _ nil))
          (try (java.util.Date/from (.toInstant (java.time.OffsetDateTime/parse s)))
               (catch Exception _ nil))
          (try (java.util.Date/from
                (.toInstant (java.time.LocalDateTime/parse s) java.time.ZoneOffset/UTC))
               (catch Exception _ nil))
          (try (java.util.Date/from
                (.toInstant (.atStartOfDay (java.time.LocalDate/parse s))
                            java.time.ZoneOffset/UTC))
               (catch Exception _ nil))
          (invalid-timestamptz! v)))
    :else (invalid-timestamptz! v)))

(defn coerce-value
  "Coerce one set-operation leaf value to the analyzer-selected common OID."
  [v target-oid]
  (cond
    (or (nil? v) (= :__null__ v)) :__null__

    (and (contains? types/array-oid->element-oid target-oid)
         (pg-arr/array? v))
    (pg-arr/array (types/cast-array-elem-kw (get types/oid->pg-name target-oid))
                  (:elements v))

    (= target-oid types/oid-timestamp)
    (cond
      (instance? java.time.LocalDate v)
      (.atStartOfDay ^java.time.LocalDate v)
      (instance? java.time.LocalDateTime v)
      v
      (instance? java.util.Date v)
      (-> ^java.util.Date v .toInstant
          (.atZone java.time.ZoneOffset/UTC) .toLocalDateTime)
      (instance? java.time.Instant v)
      (-> ^java.time.Instant v
          (.atZone java.time.ZoneOffset/UTC) .toLocalDateTime)
      :else (sql-cast/cast-scalar v "timestamp"
                                  {:explicit? true
                                   :prefer-local-datetime? true}))

    (= target-oid types/oid-timestamptz)
    (timestamptz-date v)

    :else
    (if-let [target-name (get types/oid->pg-name target-oid)]
      (sql-cast/cast-scalar v target-name {:explicit? true})
      v)))

(defn coerce-row [row result-oids]
  (if (sequential? row)
    (mapv coerce-value row result-oids)
    [(coerce-value row (first result-oids))]))
