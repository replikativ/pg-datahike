# PostgreSQL regression baseline, measured

*Measured 2026-09-21 against PostgreSQL 17's own `src/test/regress`, with
`bb pg-regress-with-fixtures` (fixtures bootstrapped, one isolated database per
run) and the diff taken against PostgreSQL's expected output with its
source-excerpt presentation lines (`LINE n:` and the caret) removed from both
sides.*

The campaign inventory in `campaign.edn` answers **how much of the schedule has
been triaged**. This answers the other question: **how much of it we actually
answer the way PostgreSQL does.**

## What we measure

`pg_regress` fails a file on any difference, so a per-file pass rate would read
zero and say nothing. Two numbers are reported instead:

- **exact API match** — our output is byte-identical to PostgreSQL's, modulo the
  parser's source excerpts. This is the real pass count.
- **agreement** — the share of PostgreSQL's own expected lines that appear, in
  order, in ours. A file at 80% answers four fifths of what PostgreSQL prints,
  which is what a partially-supported area looks like from a client.

## The numbers

| | |
|---|---|
| application-facing files in the schedule | **153** (222 scheduled, 69 out of scope) |
| measured | 151 (2 did not complete: `generated`, `test_setup`) |
| **exact API match** | **5** — `boolean`, `delete`, `md5`, `portals_p2`, `select_having` |
| 100% of PostgreSQL's lines (some extra lines of ours) | 8 |
| ≥90% | 19 |
| ≥75% | 34 |
| median agreement | **58%** |
| overall, weighted by file size | **50.6%** (100,894 of 199,529 lines) |

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

## Reproducing

```bash
bb pg-regress-with-fixtures advisory_lock       # one file
bb pg-regress-wave inventory                    # the classification, not this
```

The measurement script and per-file table live with the session that produced
them; the table below is the state on the date above.

## Per-file agreement

| file | agreement | |
|---|---|---|
| `bit` | 100% |  |
| `boolean` | 100% | exact |
| `database` | 100% |  |
| `delete` | 100% | exact |
| `md5` | 100% | exact |
| `portals_p2` | 100% | exact |
| `select_having` | 100% | exact |
| `select_implicit` | 99% |  |
| `int2` | 98% |  |
| `numeric` | 97% |  |
| `advisory_lock` | 97% |  |
| `oid` | 96% |  |
| `int4` | 95% |  |
| `money` | 95% |  |
| `enum` | 95% |  |
| `varchar` | 95% |  |
| `comments` | 94% |  |
| `int8` | 93% |  |
| `mvcc` | 90% |  |
| `transactions` | 86% |  |
| `case` | 83% |  |
| `create_aggregate` | 82% |  |
| `type_sanity` | 82% |  |
| `async` | 81% |  |
| `uuid` | 81% |  |
| `create_operator` | 80% |  |
| `char` | 79% |  |
| `errors` | 79% |  |
| `misc_sanity` | 79% |  |
| `collate` | 78% |  |
| `lseg` | 78% |  |
| `lock` | 77% |  |
| `sequence` | 76% |  |
| `time` | 76% |  |
| `drop_operator` | 74% |  |
| `copydml` | 73% |  |
| `line` | 73% |  |
| `create_type` | 73% |  |
| `float8` | 72% |  |
| `truncate` | 72% |  |
| `namespace` | 71% |  |
| `temp` | 70% |  |
| `triggers` | 69% |  |
| `limit` | 69% |  |
| `prepared_xacts` | 68% |  |
| `jsonb` | 68% |  |
| `alter_generic` | 68% |  |
| `fast_default` | 67% |  |
| `alter_operator` | 67% |  |
| `alter_table` | 67% |  |
| `foreign_key` | 67% |  |
| `strings` | 66% |  |
| `text` | 65% |  |
| `drop_if_exists` | 65% |  |
| `copy` | 65% |  |
| `psql` | 65% |  |
| `tablesample` | 64% |  |
| `rowtypes` | 64% |  |
| `merge` | 64% |  |
| `select_distinct` | 64% |  |
| `expressions` | 63% |  |
| `insert_conflict` | 63% |  |
| `create_table` | 63% |  |
| `update` | 63% |  |
| `json_encoding` | 62% |  |
| `domain` | 62% |  |
| `constraints` | 61% |  |
| `copyselect` | 60% |  |
| `macaddr` | 60% |  |
| `create_cast` | 60% |  |
| `json` | 59% |  |
| `date` | 59% |  |
| `insert` | 59% |  |
| `plancache` | 58% |  |
| `select_into` | 58% |  |
| `conversion` | 58% |  |
| `psql_crosstab` | 58% |  |
| `portals` | 57% |  |
| `arrays` | 57% |  |
| `aggregates` | 57% |  |
| `identity` | 56% |  |
| `opr_sanity` | 56% |  |
| `select` | 56% |  |
| `regex` | 56% |  |
| `prepare` | 56% |  |
| `sqljson_jsontable` | 55% |  |
| `name` | 54% |  |
| `union` | 53% |  |
| `updatable_views` | 53% |  |
| `select_distinct_on` | 52% |  |
| `misc` | 52% |  |
| `float4` | 52% |  |
| `window` | 52% |  |
| `tstypes` | 51% |  |
| `create_schema` | 51% |  |
| `create_procedure` | 51% |  |
| `create_index` | 51% |  |
| `path` | 51% |  |
| `copy2` | 50% |  |
| `with` | 49% |  |
| `timetz` | 49% |  |
| `tsearch` | 48% |  |
| `interval` | 48% |  |
| `polymorphism` | 47% |  |
| `polygon` | 47% |  |
| `macaddr8` | 46% |  |
| `tsrf` | 46% |  |
| `create_function_sql` | 46% |  |
| `inherit` | 46% |  |
| `subselect` | 46% |  |
| `random` | 45% |  |
| `matview` | 45% |  |
| `returning` | 45% |  |
| `largeobject` | 43% |  |
| `box` | 43% |  |
| `misc_functions` | 42% |  |
| `sqljson_queryfuncs` | 42% |  |
| `create_view` | 42% |  |
| `numerology` | 41% |  |
| `typed_table` | 41% |  |
| `sqljson` | 41% |  |
| `create_table_like` | 40% |  |
| `xml` | 39% |  |
| `xid` | 39% |  |
| `rules` | 39% |  |
| `join` | 38% |  |
| `create_misc` | 38% |  |
| `sysviews` | 37% |  |
| `dbsize` | 35% |  |
| `rangetypes` | 35% |  |
| `jsonpath_encoding` | 34% |  |
| `circle` | 33% |  |
| `tsdicts` | 33% |  |
| `pg_lsn` | 31% |  |
| `inet` | 28% |  |
| `txid` | 28% |  |
| `unicode` | 28% |  |
| `rangefuncs` | 28% |  |
| `regproc` | 27% |  |
| `multirangetypes` | 27% |  |
| `jsonb_jsonpath` | 26% |  |
| `timestamptz` | 24% |  |
| `point` | 22% |  |
| `jsonpath` | 20% |  |
| `explain` | 20% |  |
| `horology` | 19% |  |
| `timestamp` | 19% |  |
| `select_views` | 8% |  |
| `groupingsets` | 8% |  |
| `geometry` | 8% |  |
| `xmlmap` | 4% |  |
