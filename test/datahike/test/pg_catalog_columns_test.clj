(ns datahike.test.pg-catalog-columns-test
  "The catalog columns psql's \\d, \\d+, \\df, \\dt, \\di and the sanity
   regression files read, gated against a corpus captured from
   PostgreSQL 17 (`test/resources/catalog_columns_corpus.edn`).

   Once an unknown catalog column raised 42703 instead of answering NULL
   (#306), every query naming one of these stopped running: psql's \\d
   reported `column \"reltoastrelid\" does not exist`, \\df `prokind`,
   type_sanity `typinput`, drop_operator `oprcom`. The pg_regress
   re-measure of 2026-10-23 counted them -- psql.sql alone lost 94 lines
   to it. They are served now with the values PostgreSQL gives, the type
   and operator ones from the generated registry.

   Two wrong answers surfaced on the way and are pinned here too:
   pg_get_constraintdef / pg_get_indexdef / pg_get_triggerdef were inner
   joins, so a NULL oid REMOVED THE ROW -- \\d listed no indexes at all --
   and pg_constraint.conindid was unset, so a key constraint never met
   its index."
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer PgWireServer$QueryHandler PgWireServer$QueryResult]))

(def ^:private fixture
  (edn/read-string (slurp "test/resources/catalog_columns_corpus.edn")))

(defn- with-handler [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0 :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (f (pg/make-query-handler conn {}))
           (finally (d/release conn) (d/delete-database cfg))))))

(defn- run
  "Every row, as psql -At prints them: rows by newline, columns by |."
  [^PgWireServer$QueryHandler h ^String sql]
  (let [r (.execute h sql)]
    (if (.error ^PgWireServer$QueryResult r)
      (str "ERROR " (.error ^PgWireServer$QueryResult r))
      (str/join "\n" (map (fn [row] (str/join "|" (map #(or % "") row)))
                          (.rows ^PgWireServer$QueryResult r))))))

(deftest the-corpus-agrees-with-postgresql
  (is (<= 10 (count (:corpus fixture))) "the corpus is actually loaded")
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (doseq [sql (:setup fixture)]
        (is (nil? (.error (.execute h ^String sql))) sql))
      (doseq [[sql expected] (:corpus fixture)]
        (is (= expected (run h sql)) sql)))))

(deftest ordinary-constraint-flags-match-postgresql-18
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (doseq [sql ["CREATE TABLE parent (id integer PRIMARY KEY)"
                   "CREATE TABLE child (id integer PRIMARY KEY, pid integer UNIQUE REFERENCES parent(id), amount integer CHECK (amount > 0))"]]
        (is (nil? (.error (.execute h ^String sql))) sql))
      (is (= "c|t|0|f\nf|t|0|f\np|t|0|f\nu|t|0|f"
             (run h "SELECT contype, conenforced, conparentid, conperiod
                       FROM pg_catalog.pg_constraint
                       WHERE conrelid = 'child'::regclass AND contype <> 'n'
                       ORDER BY contype")))
      (is (= "boolean|oid|boolean"
             (run h "SELECT pg_typeof(conenforced), pg_typeof(conparentid), pg_typeof(conperiod)
                       FROM pg_catalog.pg_constraint WHERE contype = 'c'"))))))

(deftest partition-ancestors-of-ordinary-relations
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (is (nil? (.error (.execute h "CREATE TABLE ordinary (id integer)"))))
      (doseq [sql ["SELECT * FROM pg_catalog.pg_partition_ancestors('ordinary')"
                   "SELECT * FROM pg_partition_ancestors('ordinary') WITH ORDINALITY AS a(relid, depth)"
                   "SELECT pg_partition_ancestors('ordinary')"
                   "SELECT pg_partition_ancestors(NULL)"]]
        (is (= "" (run h sql)) sql))
      (is (= "t" (run h "SELECT 'ordinary'::pg_catalog.regclass IN
                                 (SELECT pg_catalog.pg_partition_ancestors('ordinary')
                                  UNION ALL VALUES ('ordinary'::pg_catalog.regclass))")))
      (is (= "42P01" (.sqlstate (.execute h "SELECT * FROM pg_partition_ancestors('missing_table')"))))
      (is (= "42883" (.sqlstate (.execute h "SELECT * FROM pg_partition_ancestors()"))))
      (is (= "42883" (.sqlstate (.execute h "SELECT pg_partition_ancestors(true)"))))
      (let [^PgWireServer$QueryResult result
            (.execute h "SELECT * FROM pg_partition_ancestors('ordinary') WITH ORDINALITY AS a(relid, depth)")]
        (is (= ["relid" "depth"] (vec (.-columnNames result))))
        (is (= [2205 20] (vec (.-columnOids result)))))
      (is (= "f|t" (run h "SELECT 1 IN (SELECT generate_series(1, 3) LIMIT 1 OFFSET 1),
                                 2 IN (SELECT generate_series(1, 3) LIMIT 1 OFFSET 1)"))))))

(deftest not-null-catalog-follows-the-advertised-major
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (let [pg18? (>= (parse-long PgWireServer/SERVER_VERSION_NUM) 180000)
            sql "SELECT c.conname, a.attname, c.connoinherit, c.conislocal,
                        c.coninhcount <> 0, c.convalidated, pg_typeof(c.conkey[1])
                   FROM pg_catalog.pg_constraint c JOIN pg_catalog.pg_attribute a
                     ON (a.attrelid = c.conrelid AND a.attnum = c.conkey[1])
                  WHERE c.contype = 'n' AND c.conrelid = 'described'::pg_catalog.regclass
                  ORDER BY a.attnum"]
        (is (nil? (.error (.execute h "CREATE TABLE described (id integer PRIMARY KEY, label text NOT NULL, notes text)"))))
        (is (= (if pg18?
                 "described_id_not_null|id|f|t|f|t|smallint\ndescribed_label_not_null|label|f|t|f|t|smallint"
                 "")
               (run h sql)))
        (is (nil? (.error (.execute h "ALTER TABLE described DROP COLUMN label"))))
        (is (= (if pg18? "described_id_not_null|id|f|t|f|t|smallint" "") (run h sql)))))))

(deftest relation-metadata-for-schema-export
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (is (nil? (.error (.execute h "CREATE TABLE exported (id integer)"))))
      (is (= "\"char\"|integer|real|integer|integer|xid|xid"
             (run h "SELECT pg_typeof(relkind), pg_typeof(relpages), pg_typeof(reltuples),
                            pg_typeof(relallvisible), pg_typeof(relallfrozen),
                            pg_typeof(relfrozenxid), pg_typeof(relminmxid)
                       FROM pg_class WHERE relname = 'exported'")))
      (is (= "0|-1|0|0|0|0"
             (run h "SELECT relpages, reltuples, relallvisible, relallfrozen,
                            relfrozenxid, relminmxid
                       FROM pg_class WHERE relname = 'exported'")))
      (is (= "0"
             (run h "SELECT CASE WHEN c.relkind = 'f'
                            THEN (SELECT ftserver FROM pg_catalog.pg_foreign_table
                                   WHERE ftrelid = c.oid)
                            ELSE 0 END
                       FROM pg_class c WHERE c.relname = 'exported'")))
      (doseq [[source expected] [["pg_am WHERE amname = 'btree'" "2601"]
                                 ["pg_language WHERE lanname = 'sql'" "2612"]
                                 ["pg_operator WHERE oid = 96" "2617"]
                                 ["pg_cast WHERE castsource = 23 AND casttarget = 20" "2605"]
                                 ["pg_tablespace WHERE spcname = 'pg_default'" "1213"]]]
        (is (= expected (run h (str "SELECT tableoid FROM " source))) source)))))

(deftest schema-qualified-built-in-casts
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (is (= "smallint|\"char\"|oid[]"
             (run h "SELECT pg_typeof(0::pg_catalog.int2),
                            pg_typeof('x'::pg_catalog.char),
                            pg_typeof('{42}'::pg_catalog.oid[])")))
      (is (= "42|oid\n43|oid"
             (run h "SELECT n, pg_typeof(n)
                       FROM unnest('{42,43}'::\"pg_catalog\".\"oid\"[]) AS a(n)
                       ORDER BY n"))))))

(deftest routine-and-type-discovery-metadata
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (is (= "10|internal|t"
             (run h "SELECT proowner, l.lanname, proacl IS NULL
                       FROM pg_proc p JOIN pg_language l ON l.oid = p.prolang
                       WHERE p.proname = 'int4eq'")))
      (is (= "10|sql"
             (run h "SELECT proowner, l.lanname
                       FROM pg_proc p JOIN pg_language l ON l.oid = p.prolang
                       WHERE p.oid = 1215")))
      (is (= "10|t"
             (run h "SELECT typowner, typacl IS NULL FROM pg_type WHERE typname = 'int4'")))
      (is (= "96|65|int4eq"
             (run h "SELECT oid, oprcode::oid, oprcode FROM pg_operator WHERE oid = 96")))
      (is (= "0" (run h "SELECT count(*) FROM pg_init_privs")))
      (is (= "0" (run h "SELECT count(*) FROM pg_transform"))))))

(deftest subscripted-name-is-a-zero-based-byte
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (is (= "\"char\"|_|t|t"
             (run h "SELECT pg_typeof(typname[0]), typname[0], typname[63] = '',
                            typname[64] IS NULL FROM pg_type WHERE typname = '_int4'")))
      (is (= "\\303|\\251|_|t"
             (run h "SELECT ('é_'::name)[0], ('é_'::name)[1], ('é_'::name)[2],
                            ('é_'::name)[-1] IS NULL")))
      (is (= "0A000" (.sqlstate (.execute h "SELECT ('abc'::name)[0:2]")))))))

(deftest built-in-operator-class-links-match-postgresql
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (is (= "2616|1978|2753|1976|2602|96|t"
             (run h "SELECT c.tableoid, c.oid, f.tableoid, f.oid, a.tableoid,
                            a.amopopr, c.opcdefault
                       FROM pg_opclass c
                       JOIN pg_opfamily f ON c.opcfamily = f.oid
                       JOIN pg_amop a ON a.amopfamily = f.oid
                       WHERE c.opcname = 'int4_ops' AND c.opcmethod = 403
                         AND a.amoplefttype = 23 AND a.amoprighttype = 23
                         AND a.amopstrategy = 3")))
      (is (= "2603|351|btint4cmp"
             (run h "SELECT tableoid, amproc::oid, amproc FROM pg_amproc
                       WHERE amprocfamily = 1976 AND amproclefttype = 23
                         AND amprocrighttype = 23 AND amprocnum = 1"))))))

(deftest catalog-shapes-for-unavailable-facilities
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (doseq [source ["pg_foreign_data_wrapper WHERE fdwowner = 10"
                      "pg_foreign_server WHERE srvfdw = 1"
                      "pg_default_acl WHERE defaclrole = 10"
                      "pg_event_trigger WHERE evtfoid = 1"
                      "pg_subscription WHERE subname = 'missing'"
                      "pg_seclabels WHERE label IS NOT NULL"
                      "pg_publication WHERE pubowner = 10 OR pubgencols = 's'"]]
        (is (= "0" (run h (str "SELECT count(*) FROM pg_catalog." source))) source)))))

(deftest catalog-options-as-rows
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (is (= "one|x=y|f|1\nbare||t|2\nempty||f|3"
             (run h "SELECT option_name, option_value, option_value IS NULL, ordinality
                       FROM pg_catalog.pg_options_to_table(ARRAY['one=x=y', 'bare', 'empty='])
                       WITH ORDINALITY ORDER BY ordinality")))
      (is (= "0" (run h "SELECT count(*) FROM pg_options_to_table(NULL::text[])")))
      (is (= "22004" (.sqlstate (.execute h "SELECT * FROM pg_options_to_table(ARRAY[NULL]::text[])"))))
      (is (= "42883" (.sqlstate (.execute h "SELECT * FROM pg_options_to_table(1)")))))))

(deftest array-subqueries-over-unqualified-outer-function-arguments
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (doseq [sql ["CREATE TABLE option_sets (id int PRIMARY KEY, opts text[])"
                   "INSERT INTO option_sets VALUES (1, ARRAY['z=last','a=first']), (2, NULL)"]]
        (is (nil? (.error (.execute h ^String sql))) sql))
      (is (= "1|{first,last}\n2|{}"
             (run h "SELECT id, ARRAY(SELECT option_value FROM pg_options_to_table(opts)
                                     ORDER BY option_name)
                       FROM option_sets ORDER BY id")))
      (is (= ""
             (run h "SELECT evtname, ARRAY(SELECT quote_literal(x) FROM unnest(evttags) AS t(x))
                       FROM pg_event_trigger"))))))

(deftest plain-index-catalog-state
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (doseq [sql ["CREATE TABLE indexed (id int PRIMARY KEY, amount int)"
                   "CREATE INDEX indexed_amount ON indexed (amount)"]]
        (is (nil? (.error (.execute h ^String sql))) sql))
      (is (= "indexed_amount|t|t|f|t|t\nindexed_pkey|t|t|f|t|t"
             (run h "SELECT c.relname, i.indisvalid, i.indisready,
                            i.indnullsnotdistinct, i.indpred IS NULL, i.indexprs IS NULL
                       FROM pg_index i JOIN pg_class c ON c.oid = i.indexrelid
                       WHERE i.indrelid = 'indexed'::regclass ORDER BY c.relname"))))))

(deftest generated-text-search-and-conversion-metadata
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (is (= "english|default|3717\nsimple|default|3717"
             (run h "SELECT c.cfgname, p.prsname, p.prsstart::oid
                       FROM pg_ts_config c JOIN pg_ts_parser p ON c.cfgparser = p.oid
                       ORDER BY c.cfgname")))
      (is (= "english_stem|snowball|dsnowball_lexize\nsimple|simple|dsimple_lexize"
             (run h "SELECT d.dictname, t.tmplname, t.tmpllexize
                       FROM pg_ts_dict d JOIN pg_ts_template t ON d.dicttemplate = t.oid
                       ORDER BY d.dictname")))
      (is (= "2607|4375|utf8_to_iso8859_1"
             (run h "SELECT tableoid, conproc::oid, conproc FROM pg_conversion
                       WHERE conforencoding = 6 AND contoencoding = 8 AND condefault"))))))

(deftest attribute-layout-and-column-locality
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (doseq [sql ["CREATE TABLE exported_parent (id integer)"
                   "CREATE TABLE exported_child (label text) INHERITS (exported_parent)"]]
        (is (nil? (.error (.execute h ^String sql))) sql))
      (is (= "id|4|i|f|f|t|t\nlabel|-1|i|t|f|t|t"
             (run h "SELECT a.attname, a.attlen, a.attalign, a.attislocal,
                            a.atthasmissing, a.attoptions IS NULL, a.attfdwoptions IS NULL
                       FROM pg_attribute a
                       WHERE a.attrelid = 'exported_child'::regclass
                       ORDER BY a.attnum"))))))

(deftest tableoid-follows-left-join-row-presence
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (doseq [sql ["CREATE TABLE tableoid_left (id int)"
                   "CREATE TABLE tableoid_right (id int)"
                   "INSERT INTO tableoid_left VALUES (1), (2)"
                   "INSERT INTO tableoid_right VALUES (1)"]]
        (is (nil? (.error (.execute h ^String sql))) sql))
      (is (= "1|tableoid_right|f\n2||t"
             (run h "SELECT l.id, c.relname, r.tableoid IS NULL
                       FROM tableoid_left l LEFT JOIN tableoid_right r
                         ON r.id = l.id AND r.tableoid = 'tableoid_right'::regclass
                       LEFT JOIN pg_class c ON c.oid = r.tableoid
                       ORDER BY l.id"))))))
