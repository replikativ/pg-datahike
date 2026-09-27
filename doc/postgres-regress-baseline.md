# PostgreSQL regression baseline, measured

*Measured 2026-09-27 against PostgreSQL 17's own `src/test/regress`, with
`bb pg-regress-with-fixtures` (fixtures bootstrapped, one isolated database per
run) and the diff taken against PostgreSQL's expected output with its
source-excerpt presentation lines (`LINE n:` and the caret) removed from both
sides.*

The campaign inventory in `campaign.edn` answers **how much of the schedule has
been triaged**. This answers the other question: **how much of it we actually
answer the way PostgreSQL does.**

## What we measure

`pg_regress` fails a file on any difference, so a per-file pass rate would read
zero and say nothing. **Agreement** is measured instead: the share of
PostgreSQL's own expected lines that appear, in order, in ours. A file at 80%
answers four fifths of what PostgreSQL prints, which is what a
partially-supported area looks like from a client.

100% means every expected line is present and in order, not that the output is
byte-identical -- extra lines of ours do not lower it.

The per-file numbers live in `test/integration/postgres-regress/agreement.edn`
and are enforced by `bb pg-regress-gate`; see "Keeping it" below.

## The numbers

| | |
|---|---|
| application-facing files | **179** (245 regression files, 65 out of scope, 1 not measurable) |
| at 100% of PostgreSQL's expected lines | **9** |
| ≥90% | 22 |
| ≥75% | 42 |
| ≥50% | 117 |
| median agreement | **59.5%** |

Measured again on 2026-09-27 after the routine work (SQL functions, plpgsql,
triggers, notices). Against the 150 files common to the 2026-09-21 run, 20
improved and 19 lost ground, with the median flat.

Most of that movement is **not** real. A file whose output is mostly cascade
realigns by a point or two whenever anything ahead of its first divergence
changes, in either direction, so per-file agreement is noisy at the
few-percent level. That is why the gate has a tolerance, and why a real
regression is recognised by being several points, or by a file dropping off
100%.

One of the nineteen was real, and worth the whole exercise: `date` fell 15
points, and the cause was a **silent wrong answer** that predated this work.
`'1997-13-01'::date` answered the string `1997-13-01` -- a date with a
thirteenth month -- and `'1997-04-31'` quietly became the 30th. Fixed; `date`
is now 60.9%.

The biggest genuine gains since the first measurement:

| file | 2026-09-21 | 2026-09-27 |
|---|---|---|
| `select_distinct_on` | 52% | 72% |
| `select` | 56% | 72% |
| `expressions` | 63% | 77% |
| `plancache` | 58% | 67% |
| `polymorphism` | 47% | 54% |
| `drop_if_exists` | 65% | 70% |

## Keeping it

```bash
bb pg-regress-gate      # measure, and fail if any file lost ground
bb pg-regress-measure   # re-measure and rewrite agreement.edn after a gain
```

Both run all 179 files and take about ninety minutes, which is why the gate is
**not** in per-PR CI: a ninety-minute job on every push would cost more than it
catches, given that the differential fuzzer and the unit suite already run
there and catch regressions in minutes. Run it before and after a change that
touches a broad surface, and whenever a slice of this document's residual is
worked on.

## What accounts for the other half

Errors we raise across the corpus, by class:

| class | errors | files |
|---|---|---|
| SQL parse error (JSqlParser rejects the syntax) | 3072 | 93 |
| relation does not exist (mostly a cascade from an earlier failure) | 2555 | 72 |
| function does not exist | 2452 | 92 |
| transaction aborted (a cascade of the above) | 707 | 33 |
| unsupported statement (MERGE, and others JSqlParser parses but we refuse) | 564 | 28 |
| `CREATE FUNCTION` | 549 | 67 |
| type does not exist | 474 | 40 |
| partitioning (`PARTITION BY`, `ATTACH PARTITION`) | 442 | 22 |
| column does not exist | 426 | 49 |
| `CREATE TRIGGER` | 289 | 18 |
| `DROP FUNCTION` | 241 | 42 |
| `CREATE RULE` | 134 | 15 |

The largest single lever is **the parser**: 3072 statements never reach the
translator. The syntax it rejects, by frequency, is SQL/JSON (`JSON_EXISTS`,
`JSON_VALUE`, `JSON_TABLE`), XML (`xmlserialize`, `xmlparse`), window-frame
`EXCLUDE`, `INSERT … DEFAULT VALUES`, interval qualifiers (`interval '1'
minute to second`), and the partitioning grammar.

The second is **server-side routines**: `CREATE FUNCTION` / `TRIGGER` / `RULE`
account for 972 errors across 82 files, and they cascade — a file that cannot
create its helper function then fails every statement that calls it.

Two smaller, self-contained ones:

- **Notices.** PostgreSQL prints 1352 `NOTICE` / `WARNING` / `INFO` lines across
  73 files that we never send, because the wire layer has no `NoticeResponse`.
  `advisory_lock` is at 268 of 276 lines and *every* remaining difference is a
  missing WARNING.
- **Missing functions**, led by range constructors (`numrange`, `int4range`,
  `daterange`), `to_timestamp` / `to_date` (the `formatting.c` template
  engine, already Phase 6), and text search (`to_tsquery`,
  `websearch_to_tsquery`).

## Known divergences we accept

- **Row order without ORDER BY.** A materialised relation -- a derived table, a
  CTE, a set operation, a function used as a relation -- returns its rows in
  the order the store scans them, not the order its body produced. SQL does not
  promise an order without ORDER BY, PostgreSQL happens to preserve one, and
  its expected output records what PostgreSQL happened to do. Preserving it
  would mean carrying an ordinal on every materialised relation and sorting by
  it, on a path that is otherwise a scan. Measured cost of not doing it:
  row-order-only differences are 28 lines of the 88,321 missing.

## Reproducing

```bash
bb pg-regress-with-fixtures advisory_lock       # one file
bb pg-regress-wave inventory                    # the classification, not this
```

The measurement script and per-file table live with the session that produced
them; the table below is the state on the date above.

## Per-file agreement

The authoritative copy is `test/integration/postgres-regress/agreement.edn`,
which the gate reads. This table is a snapshot for reading.

| file | agreement |
|---|---|
| `bit` | 100% |
| `boolean` | 100% |
| `database` | 100% |
| `delete` | 100% |
| `md5` | 100% |
| `portals_p2` | 100% |
| `reindex_catalog` | 100% |
| `select_having` | 100% |
| `select_implicit` | 99% |
| `numeric_big` | 98% |
| `int2` | 98% |
| `numeric` | 97% |
| `advisory_lock` | 97% |
| `enum` | 96% |
| `oid` | 96% |
| `int4` | 96% |
| `money` | 95% |
| `varchar` | 95% |
| `int8` | 94% |
| `comments` | 94% |
| `mvcc` | 90% |
| `transactions` | 90% |
| `case` | 84% |
| `copyencoding` | 82% |
| `create_aggregate` | 82% |
| `type_sanity` | 82% |
| `collate` | 81% |
| `collate.utf8` | 81% |
| `async` | 81% |
| `uuid` | 81% |
| `collate.windows.win1252` | 80% |
| `create_operator` | 80% |
| `errors` | 80% |
| `char` | 79% |
| `misc_sanity` | 79% |
| `collate.linux.utf8` | 78% |
| `lseg` | 78% |
| `expressions` | 77% |
| `lock` | 77% |
| `sequence` | 76% |
| `time` | 76% |
| `drop_operator` | 74% |
| `copydml` | 73% |
| `without_overlaps` | 73% |
| `create_type` | 73% |
| `line` | 73% |
| `temp` | 73% |
| `select_distinct_on` | 73% |
| `select` | 72% |
| `float8` | 72% |
| `prepared_xacts` | 72% |
| `truncate` | 72% |
| `fast_default` | 71% |
| `select_distinct` | 71% |
| `namespace` | 71% |
| `drop_if_exists` | 70% |
| `triggers` | 70% |
| `limit` | 69% |
| `euc_kr` | 69% |
| `jsonb` | 68% |
| `alter_generic` | 68% |
| `alter_table` | 67% |
| `plancache` | 67% |
| `alter_operator` | 67% |
| `create_cast` | 67% |
| `foreign_key` | 67% |
| `compression_pglz` | 66% |
| `strings` | 66% |
| `encoding` | 66% |
| `psql` | 65% |
| `text` | 65% |
| `copy` | 65% |
| `tablesample` | 64% |
| `rowtypes` | 64% |
| `stats_import` | 64% |
| `domain` | 64% |
| `collate.icu.utf8` | 64% |
| `merge` | 64% |
| `create_table` | 63% |
| `insert_conflict` | 63% |
| `update` | 63% |
| `for_portion_of` | 62% |
| `json_encoding` | 62% |
| `numa` | 62% |
| `date` | 61% |
| `constraints` | 60% |
| `nls` | 60% |
| `copyselect` | 60% |
| `compression_lz4` | 60% |
| `planner_est` | 60% |
| `macaddr` | 60% |
| `graph_table` | 60% |
| `json` | 59% |
| `insert` | 59% |
| `stats_rewrite` | 58% |
| `select_into` | 58% |
| `conversion` | 58% |
| `psql_crosstab` | 58% |
| `portals` | 58% |
| `aggregates` | 57% |
| `arrays` | 57% |
| `identity` | 56% |
| `opr_sanity` | 56% |
| `regex` | 56% |
| `prepare` | 56% |
| `sqljson_jsontable` | 55% |
| `polymorphism` | 55% |
| `name` | 54% |
| `psql_pipeline` | 54% |
| `union` | 53% |
| `updatable_views` | 53% |
| `misc` | 52% |
| `window` | 52% |
| `float4` | 52% |
| `tstypes` | 51% |
| `create_schema` | 51% |
| `create_procedure` | 51% |
| `create_index` | 51% |
| `path` | 50% |
| `copy2` | 50% |
| `with` | 49% |
| `timetz` | 49% |
| `tsearch` | 48% |
| `create_function_sql` | 48% |
| `interval` | 48% |
| `polygon` | 47% |
| `subselect` | 47% |
| `macaddr8` | 46% |
| `generated_virtual` | 46% |
| `tsrf` | 46% |
| `pg_ndistinct` | 46% |
| `pg_dependencies` | 46% |
| `inherit` | 46% |
| `random` | 45% |
| `matview` | 45% |
| `returning` | 45% |
| `generated_stored` | 44% |
| `largeobject` | 43% |
| `box` | 43% |
| `misc_functions` | 42% |
| `sqljson_queryfuncs` | 42% |
| `create_view` | 42% |
| `numerology` | 42% |
| `typed_table` | 41% |
| `graph_table_rls` | 41% |
| `create_table_like` | 41% |
| `sqljson` | 41% |
| `xml` | 39% |
| `xid` | 39% |
| `rules` | 39% |
| `create_misc` | 38% |
| `sysviews` | 37% |
| `create_property_graph` | 37% |
| `dbsize` | 36% |
| `rangetypes` | 35% |
| `join` | 35% |
| `jsonpath_encoding` | 34% |
| `circle` | 33% |
| `tsdicts` | 33% |
| `rangefuncs` | 32% |
| `oid8` | 32% |
| `pg_lsn` | 31% |
| `inet` | 28% |
| `txid` | 28% |
| `unicode` | 28% |
| `regproc` | 27% |
| `multirangetypes` | 27% |
| `eager_aggregate` | 26% |
| `jsonb_jsonpath` | 26% |
| `timestamptz` | 24% |
| `point` | 22% |
| `jsonpath` | 20% |
| `explain` | 20% |
| `horology` | 20% |
| `timestamp` | 19% |
| `select_views` | 9% |
| `groupingsets` | 8% |
| `geometry` | 8% |
| `xmlmap` | 4% |
