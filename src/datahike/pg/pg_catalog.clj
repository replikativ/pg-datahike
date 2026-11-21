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
   :typispreferred :typdelim :typelem :typarray :typcollation, and the
   I/O and storage columns :typinput :typoutput :typreceive :typsend
   :typmodin :typmodout :typanalyze :typsubscript (regproc names, \"-\"
   for none) :typalign :typbyval :typstorage. Category and type are
   one-letter strings, as in the catalog."
  (rows :types))

(def casts
  "pg_cast rows: :source :target :context (i/a/e) :method (f/i/b) :func."
  (rows :casts))

(def procs
  "pg_proc rows: :oid :proname :args (OIDs) :ret :kind (f/a/w/p)
   :strict :volatile (i/s/v) :retset :variadic (0 when not variadic)
   :language (the pg_language OID)."
  (let [languages (into {} (map (juxt :oid :language)) (rows :proc-languages))]
    (mapv #(assoc % :language (get languages (:oid %) 12)) (rows :procs))))

(def operators
  "pg_operator rows: :oid :oprname :kind (b/l) :left (0 for prefix)
   :right :result :code :com and :negate (operator OIDs, 0 for none)
   :rest and :join (regproc names, \"-\" for none)."
  (rows :operators))

(def opclasses (rows :opclasses))
(def opfamilies (rows :opfamilies))
(def amops (rows :amops))
(def amprocs (rows :amprocs))

(def conversions (rows :conversions))

;; initdb creates Snowball after the bootstrap catalog. Use its PG17
;; identities for the English configuration implemented by pg-datahike.
(def snowball-procs
  [{:oid 13258 :proname "dsnowball_init" :args [2281] :ret 2281 :kind "f"
    :strict true :volatile "v" :retset false :variadic 0 :language 13}
   {:oid 13259 :proname "dsnowball_lexize" :args [2281 2281 2281 2281] :ret 2281 :kind "f"
    :strict true :volatile "v" :retset false :variadic 0 :language 13}])

(def ts-parsers (rows :ts-parsers))

(def ts-templates
  (conj (rows :ts-templates)
        {:oid 13260 :tmplname "snowball" :tmplnamespace 11
         :tmplinit 13258 :tmpllexize 13259}))

(def ts-dicts
  (conj (rows :ts-dicts)
        {:oid 13273 :dictname "english_stem" :dictnamespace 11 :dictowner 10
         :dicttemplate 13260 :dictinitoption "language = 'english', stopwords = 'english'"}))

(def ts-configs
  (conj (rows :ts-configs)
        {:oid 13274 :cfgname "english" :cfgnamespace 11 :cfgowner 10 :cfgparser 3722}))

(def type-by-oid (into {} (map (juxt :oid identity)) types))

(def type-by-name (into {} (map (juxt :typname identity)) types))

(def procs-by-name (group-by :proname procs))
