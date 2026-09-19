(ns datahike.pg.gen.catalog
  "Generates src/datahike/pg/pg_catalog.edn from a PostgreSQL source
   checkout's catalog data files (src/include/catalog/*.dat), the same
   files genbki.pl builds the bootstrap catalogs from.

   The catalog tables pg-datahike resolves against -- types, casts,
   functions, operators -- come from here and only from here, so they
   cannot drift from the PostgreSQL release we claim to be. Regenerate
   with `bb gen-catalog [path-to-postgres-source]` after moving to a new
   release; `pg-catalog-generated-test` fails when the committed file
   was edited by hand, or when it disagrees with a checkout present at
   the default path.

   Only the columns resolution needs are kept, and type references are
   resolved to OIDs the way genbki's BKI_LOOKUP does."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def default-source ".internal/postgres-REL_17_7")

(def output-path "src/datahike/pg/pg_catalog.edn")

;; ---------------------------------------------------------------------------
;; .dat reader
;;
;; A .dat file is a Perl array of hashes: `{ key => 'value', ... },`.
;; Values are single-quoted with backslash escapes; `#` starts a comment
;; outside a quoted string.

(defn- parse-dat [^String text]
  (let [n (count text)]
    (loop [i 0, row nil, rows (transient [])]
      (if (>= i n)
        (persistent! rows)
        (let [c (.charAt text i)]
          (cond
            (= c \#) (let [e (str/index-of text "\n" i)]
                       (recur (if e (inc e) n) row rows))
            (= c \{) (recur (inc i) {} rows)
            (= c \}) (recur (inc i) nil (conj! rows row))
            (and row (Character/isLetter c))
            (let [m (re-matcher #"\G([a-z_0-9]+)\s*=>\s*'" text)]
              (.region m i n)
              (when-not (.lookingAt m)
                (throw (ex-info "unparseable .dat entry" {:at i})))
              (let [k (keyword (.group m 1))
                    sb (StringBuilder.)
                    end (loop [j (.end m)]
                          (let [ch (.charAt text j)]
                            (cond
                              (= ch \\) (do (.append sb (.charAt text (inc j))) (recur (+ j 2)))
                              (= ch \') (inc j)
                              :else (do (.append sb ch) (recur (inc j))))))]
                (recur end (assoc row k (str sb)) rows)))
            :else (recur (inc i) row rows)))))))

(defn- read-dat [src table]
  (parse-dat (slurp (io/file src "src/include/catalog" (str table ".dat")))))

;; ---------------------------------------------------------------------------
;; Tables

(def ^:private collation-oids
  "pg_collation.dat: the collations pg_type.dat refers to by name."
  {"default" 100 "C" 950 "POSIX" 951})

(defn- ->bool [s] (= "t" s))

(def ^:private build-constants
  "The symbols pg_type.dat writes for a length, with the values of a
   default 64-bit build (pg_config_manual.h, configure)."
  {"NAMEDATALEN" 64 "SIZEOF_POINTER" 8})

(defn- types
  "pg_type rows, with the array types genbki generates from
   `array_type_oid` (Catalog.pm GenerateArrayTypes: `_name`, category A,
   varlena, element's collation and delimiter)."
  [src]
  (let [rows (read-dat src "pg_type")
        base (for [r rows]
               {:oid (parse-long (:oid r))
                :typname (:typname r)
                :typlen (or (build-constants (:typlen r)) (parse-long (:typlen r)))
                :typtype (or (:typtype r) "b")
                :typcategory (:typcategory r)
                :typispreferred (->bool (:typispreferred r "f"))
                :typdelim (:typdelim r ",")
                :typelem (:typelem r)
                :typcollation (get collation-oids (:typcollation r) 0)
                :array-oid (some-> (:array_type_oid r) parse-long)
                :typarray (:typarray r)})
        arrays (for [t base :when (:array-oid t)]
                 {:oid (:array-oid t)
                  :typname (str "_" (:typname t))
                  :typlen -1
                  :typtype "b"
                  :typcategory "A"
                  :typispreferred false
                  :typdelim (:typdelim t)
                  :typelem (:typname t)
                  :typcollation (:typcollation t)})
        all (concat base arrays)
        name->oid (into {} (map (juxt :typname :oid)) all)]
    (->> all
         (map (fn [t]
                (-> t
                    (dissoc :array-oid)
                    (update :typelem #(if % (name->oid %) 0))
                    ;; array_type_oid, or an explicit typarray (record)
                    (assoc :typarray (or (:array-oid t)
                                         (some-> (:typarray t) name->oid)
                                         0)))))
         (sort-by :oid)
         vec)))

(defn- type-lookup
  "genbki's BKI_LOOKUP(pg_type): a type name to its OID; `-`/`0` is none."
  [types]
  (let [m (into {} (map (juxt :typname :oid)) types)]
    (fn [s]
      (cond (or (nil? s) (= "0" s) (= "-" s)) 0
            (re-matches #"\d+" s) (parse-long s)
            :else (or (get m s) (throw (ex-info "unknown type name" {:name s})))))))

(defn- casts [src type-oid]
  (->> (read-dat src "pg_cast")
       (map (fn [r] {:source (type-oid (:castsource r))
                     :target (type-oid (:casttarget r))
                     :context (:castcontext r)
                     :method (:castmethod r)
                     :func (:castfunc r)}))
       (sort-by (juxt :source :target))
       vec))

(defn- procs [src type-oid]
  (->> (read-dat src "pg_proc")
       (map (fn [r]
              (let [args (if (str/blank? (:proargtypes r))
                           []
                           (mapv type-oid (str/split (str/trim (:proargtypes r)) #"\s+")))]
                {:oid (parse-long (:oid r))
                 :proname (:proname r)
                 :args args
                 :ret (type-oid (:prorettype r))
                 :kind (:prokind r "f")
                 :strict (->bool (:proisstrict r "t"))
                 :volatile (:provolatile r "i")
                 :retset (->bool (:proretset r "f"))
                 :variadic (type-oid (:provariadic r "0"))})))
       (sort-by :oid)
       vec))

(defn- operators [src type-oid]
  (->> (read-dat src "pg_operator")
       (map (fn [r] {:oid (parse-long (:oid r))
                     :oprname (:oprname r)
                     :kind (:oprkind r "b")
                     :left (type-oid (:oprleft r "0"))
                     :right (type-oid (:oprright r))
                     :result (type-oid (:oprresult r))
                     :code (:oprcode r)}))
       (sort-by :oid)
       vec))

(defn- source-version [src]
  (let [configure (slurp (io/file src "configure.ac"))]
    (second (re-find #"AC_INIT\(\[PostgreSQL\], \[([^\]]+)\]" configure))))

(defn generate
  "The catalog map for the PostgreSQL checkout at `src`."
  [src]
  (let [ts (types src)
        type-oid (type-lookup ts)]
    {:pg-version (source-version src)
     :types ts
     :casts (casts src type-oid)
     :procs (procs src type-oid)
     :operators (operators src type-oid)}))

(def columns
  "Column order of each table's rows in the generated file."
  {:types [:oid :typname :typlen :typtype :typcategory :typispreferred
           :typdelim :typelem :typarray :typcollation]
   :casts [:source :target :context :method :func]
   :procs [:oid :proname :args :ret :kind :strict :volatile :retset :variadic]
   :operators [:oid :oprname :kind :left :right :result :code]})

(defn sha256 [^String s]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" %) (.digest md (.getBytes s "UTF-8"))))))

(defn render-body
  "Everything after the checksum line: one row per line, as a vector in
   `columns` order, so the file stays small and release diffs readable."
  [catalog]
  (str (str/join
        "\n"
        (for [[k cols] columns]
          (str " " k "\n {:columns " (pr-str cols) "\n  :rows\n  ["
               (str/join "\n   " (for [row (get catalog k)]
                                    (pr-str (mapv row cols))))
               "]}")))
       "}\n"))

(defn render
  "The file text. The checksum covers the body, so an edit by hand shows
   without the PostgreSQL source at hand."
  [catalog]
  (let [body (render-body catalog)]
    (str ";; Generated by datahike.pg.gen.catalog from PostgreSQL "
         (:pg-version catalog) " src/include/catalog/*.dat.\n"
         ";; Do not edit: run `bb gen-catalog`.\n"
         "{:pg-version " (pr-str (:pg-version catalog)) "\n"
         " :body-sha256 " (pr-str (sha256 body)) "\n"
         body)))

(defn split-file
  "[checksum body] of a generated file's text."
  [^String text]
  (let [[_ sha body] (re-find #"(?s)\n :body-sha256 \"([0-9a-f]+)\"\n(.*)\z" text)]
    [sha body]))

(defn -main [& [src]]
  (let [src (or src default-source)
        catalog (generate src)]
    (spit output-path (render catalog))
    (println "wrote" output-path "from PostgreSQL" (:pg-version catalog) "--"
             (count (:types catalog)) "types," (count (:casts catalog)) "casts,"
             (count (:procs catalog)) "functions," (count (:operators catalog)) "operators")))
