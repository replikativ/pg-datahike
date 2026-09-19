(ns datahike.pg.pg-catalog
  "PostgreSQL's built-in catalog rows -- pg_type, pg_cast, pg_proc,
   pg_operator -- as generated from the release's catalog data files
   (see datahike.pg.gen.catalog). Type, cast, function and operator
   resolution read these; nothing here is written by hand."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def ^:private raw
  (edn/read-string (slurp (io/resource "datahike/pg/pg_catalog.edn"))))

(def pg-version
  "The PostgreSQL release the rows come from."
  (:pg-version raw))

(defn- rows [k]
  (let [{:keys [columns rows]} (get raw k)]
    (mapv #(zipmap columns %) rows)))

(def types
  "pg_type rows: :oid :typname :typlen :typtype :typcategory
   :typispreferred :typdelim :typelem :typarray :typcollation. Category
   and type are one-letter strings, as in the catalog."
  (rows :types))

(def casts
  "pg_cast rows: :source :target :context (i/a/e) :method (f/i/b) :func."
  (rows :casts))

(def procs
  "pg_proc rows: :oid :proname :args (OIDs) :ret :kind (f/a/w/p)
   :strict :volatile (i/s/v) :retset :variadic (0 when not variadic)."
  (rows :procs))

(def operators
  "pg_operator rows: :oid :oprname :kind (b/l) :left (0 for prefix)
   :right :result :code."
  (rows :operators))

(def type-by-oid (into {} (map (juxt :oid identity)) types))

(def type-by-name (into {} (map (juxt :typname identity)) types))

(def procs-by-name (group-by :proname procs))
