# PostgreSQL regression baseline

The default regression profile is PostgreSQL 17.7 (`REL_17_7`). The
PostgreSQL 18.6 (`REL_18_6`) profile has its own inventory and strict gate.
The schedule inventory records which files have been classified. The output
comparison helps locate differences in SQL behavior. Neither is a percentage
of PostgreSQL features implemented.

## What the comparison measures

Agreement is the share of PostgreSQL's expected output lines that appear,
in order, in pg-datahike's output. Only source excerpts (`LINE n:` and
carets) are removed before comparison. Commands, rows, errors and notices
remain.

A score of 100% means every expected line occurs in order. It does **not**
mean the outputs match exactly: extra output does not lower the score.
Echoed SQL, formatting and cascading errors also contribute to the metric.
Use it to find a file and its first meaningful difference; verify behavior
with statement-level differential tests.

The authoritative per-file scores are in
[`agreement.edn`](../test/integration/postgres-regress/agreement.edn).
The strict admissions in
[`campaign.edn`](../test/integration/postgres-regress/campaign.edn)
use a stronger comparison of complete normalized output.

## Recorded baseline: 2026-10-24

The baseline recorded in PR #350 was measured against PostgreSQL 17.7 before
the additional fixture prerequisites in PR #354.

| Measure | Recorded value |
|---|---:|
| Files in the pinned parallel schedule | 222 |
| Scheduled files classified as out of scope | 69 |
| Application-facing scheduled files | 153 |
| Scheduled files measured | 152; `test_setup` runs as the bootstrap |
| Additional files measured | `numeric_big` and `reindex_catalog` |
| Total files measured | 154 |
| Files with 100% expected-line coverage | 16 |
| Mean per-file agreement | 64.4% |
| Approximate line-weighted agreement | 55.5% |

The agreement runner also discovers SQL files outside the parallel schedule.
Its denominator therefore differs from the schedule inventory. The pinned
SQL directory contains 224 files; after excluding the 69 files in
`scope.edn`, 155 are candidates, of which 154 have recorded scores;
`test_setup` has no independent score because it runs as the bootstrap.
A missing result must remain unmeasured rather than reuse an older artifact.

The line-weighted estimate represents about 81,700 unmatched expected lines
out of 183,800. It gives larger files more weight than the per-file mean;
both measures include presentation and fixture effects as well as semantic
differences.

| Expected-line coverage | Files |
|---|---:|
| 100% | 16 |
| 90–99% | 11 |
| 70–89% | 37 |
| 50–69% | 51 |
| Below 50% | 39 |

Separate probes with corrected prerequisites raised `geometry` from 7.2%
to 14.9% and `horology` from 37.8% to 49.0%. These are fixture corrections,
not changes in server behavior. They are not incorporated into the recorded
aggregate baseline above.

## Remaining compatibility areas

The estimates below come from classifying errors in the recorded outputs.
They overlap: a rejected CREATE can cause many missing-relation and
transaction-aborted errors. PostgreSQL deliberately raises errors in its own
tests too. These counts guide investigation; they do not count independent
defects or predict how much a feature will improve agreement.

| Approximate unmatched lines | Area | Next investigation |
|---|---|---|
| 19,200 | Cascading failures | Find the first failed statement and distinguish missing fixtures from unsupported behavior |
| 12,800 | Parser gaps | Expression indexes, window GROUPS/EXCLUDE, SEARCH/CYCLE, array assignment, DDL and geometric operators |
| 7,400 | Missing functions | JSON path functions, record population, range constructors and tsquery functions |
| 4,900 | SQL/JSON and JSON path | Query/value/table expressions and path evaluation |
| 4,700 | Ranges and multiranges | Types, constructors and operators |
| 3,100 | DDL and routines | Classify unsupported forms of functions, operators, aggregates, rules and ALTER statements individually |
| 3,100 | Name and join resolution | Parenthesized joins, qualified names and missing FROM references |
| 3,000 | EXPLAIN output | Separate SQL API requirements from PostgreSQL planner presentation |
| 2,600 | Text search | Query construction and remaining operators |
| 1,600 | Partitioning | Establish the intended routing and catalog contract |
| 1,500 | XML | Decide the supported surface |
| 1,400 | Remaining types | regproc family, inet/cidr and transaction identifiers |
| 700 | MERGE | Action selection and effects |
| 15,700 | Other errors | Compare individual statements, including expected errors |

Support decisions belong to individual capabilities. A file's scope
classification does not establish whether every statement in it should be
supported.

## Scope

[`scope.edn`](../test/integration/postgres-regress/scope.edn) excludes 69
scheduled files covering PostgreSQL physical access methods, planner and
storage internals, physical partitioning tests, server extension hooks,
administration and security, replication, internal sanity checks and
platform collation matrices.

Some excluded files also contain useful application-facing behavior. For
example, PL/pgSQL execution is implemented, but the entire upstream
`plpgsql` file is not a compatibility gate. Focused differential tests must
establish the supported subset.

Row order without ORDER BY can also differ. Investigate ordering required
by a SQL construct independently from incidental order in expected output;
an aggregate's ORDER BY is part of its contract.

## PostgreSQL 18.6

The live whole-file gate includes `advisory_lock`, `boolean`, `comments`,
`delete`, `lseg`, `macaddr`, `md5`, `select_having` and `select_implicit`.
Each matched a fresh native 18.6 oracle and pg-datahike v0.1.390 after removing
source excerpts and their caret markers. The gate uses the pinned upstream
output and gives each file a separate database. `numeric_big` and `portals_p2`
also run in separate databases with the minimal setup declared in `fixtures.edn`.
Focused tests also cover
upstream `CREATE TABLE` and `CREATE TABLE LIKE` slices, including catalog
state after rejected statements.

| Discovery case | Observed gaps |
|---|---|
| `aggregates` | Custom aggregate fixtures cause cascades. Built-in aggregate coverage and resolution remain open. |
| `arrays` | Array input, type resolution, collated and other `array_sort` orderings, `array_sample` and `array_reverse`. Numeric `array_sort` has strict slices for ordering, dimensions and bounds. |
| `copyselect` | COPY diagnostics. Strict wire slices check legacy `FORCE QUOTE` CSV output and successive `COPY FROM STDIN` commands. |
| `create_table` and `create_table_like` | Partitioning, some persistence metadata and optional LIKE properties such as `INCLUDING GENERATED`. A subquery in a default is rejected with a different SQLSTATE. |
| `groupingsets` | Grouping sets, rollup and cube are unsupported. The v0.1.390 replay completed with early rejections; the initial October run exhausted the heap. |
| `prepare` | Arity, declared type coercion and unknown-type errors now agree in focused probes. A strict slice covers `CREATE TABLE AS EXECUTE`; path inference and catalog output still differ. |

The October intake attempted the 164 selected files. The native run completed
164 and the then-current pg-datahike build completed 162. The two incomplete
target cases were `groupingsets` and `database`, where unsupported database
rename left a run-created database behind. Global database DDL discovery
requires an exclusive disposable endpoint. The full agreement manifest
remains unqualified. Use [the campaign manifest](../test/integration/postgres-regress/pg18/campaign.edn)
for admissions and per-file blockers, and [the fixture descriptor](../test/integration/postgres-regress/pg18/fixtures.edn)
for prerequisite and native-helper boundaries.

The [residual map](../test/integration/postgres-regress/pg18/residuals.edn)
records the later paired discovery across all 164 application-facing files.
It tracks output outcomes and fixture readiness independently. Findings link
to upstream SQL ranges and distinguish wrong results, internal errors,
unsupported SQL and fixture boundaries. A covered finding must name a live
strict slice. Files without a recorded cause remain visible in the census.

```bash
PG_REGRESS_PROFILE=18 bb pg-regress-wave residuals
```

Run the current gate with PostgreSQL 18 clients and a pg-datahike server with
database provisioning enabled:

```bash
PG_REGRESS_PROFILE=18 bb pg-regress-setup
PG_REGRESS_PROFILE=18 bb pg-regress-wave strict
```

### PostgreSQL 18 client cutover

The advertised server version remains 17.7. PostgreSQL 18 client behavior
is checked separately from the strict SQL regression profile. The existing
`pgdump-roundtrip` CI job uses native PostgreSQL as the dump source and
pg-datahike as the restore target.

Load the following into disposable native and pg-datahike databases to
reproduce the client probes. For the candidate only, build a throwaway copy
with `SERVER_VERSION` set to `18.6` and `SERVER_VERSION_NUM` to `180006` in
`PgWireServer.java` before compiling Java.

```sql
CREATE TABLE client_parent (id int PRIMARY KEY, name text NOT NULL);
CREATE TABLE client_child (
  id int PRIMARY KEY,
  parent_id int REFERENCES client_parent,
  amount int CHECK (amount > 0)
);
CREATE INDEX client_child_amount_idx ON client_child (amount);
CREATE VIEW client_view AS SELECT id FROM client_child;
```

Point `PGHOST`, `PGPORT`, `PGUSER`, and `PGDATABASE` at each endpoint.
Run both commands with PostgreSQL 18 client binaries.

```bash
psql -X -E -v ON_ERROR_STOP=1 -c '\d client_child' -c '\d+ client_child'
pg_dump --schema-only --no-owner --no-privileges > fixture-schema.sql
```

The first changes cover `pg_constraint.conperiod`, `conenforced`,
`conparentid`, the typed extension configuration columns, and the typed
namespace ACL column. The schema-only dump now reaches `pg_class.relpages`.
Basic `\d` on the 18.6 candidate shows both tables, the view, the primary
and secondary indexes, the check, and both directions of the foreign-key
relationship. That path exercises a set-returning function and `UNION ALL
VALUES` for foreign-key discovery. Detailed `\d+` also succeeds on the
ordinary-table fixture after correlated `unnest` and array-element joins
were fixed. The 18.6 candidate reports `NOT NULL` constraints in
`pg_constraint` (`contype = 'n'`) and links them to the correct physical
column positions. The current metadata derives the PostgreSQL names for
unnamed constraints. Explicit `NOT NULL` constraint names need durable
metadata before they can be reported faithfully.

A discovery run captured the native dump SQL with
`PGOPTIONS='-c log_statement=all' pg_dump --schema-only` against the fixture.
Of 48 read queries replayed separately on the 18.6 candidate after the first
constraint and extension fix, 32 errored. These are query counts from that
revision. Native object IDs appeared in some queries, so empty results in
the replay do not establish parity.

| SQLSTATE | Queries | Next investigation |
|---|---:|---|
| `42P01` | 17 | Missing catalog relations, including `pg_init_privs`, `pg_opclass`, `pg_amop`, `pg_transform`, foreign data and text search catalogs |
| `42703` | 8 | Missing fields and system columns, including `relpages`, `proacl`, `pubowner`, and `tableoid` on several catalogs |
| `0A000` | 6 | OID-array `unnest` and correlated catalog projections |
| `42804` | 1 | `pg_type` scan with an internal `"char"` CASE branch |

The cutover needs successful stock-client describe and schema export on
this fixture, an export restored into an empty native database with the
same tables, view, index and constraints, and a passing PostgreSQL 18
regression gate on the candidate build.

## Running the comparison

```bash
bb pg-regress-wave inventory       # schedule classification and admissions
bb pg-regress-gate                 # compare all results with the baseline
bb pg-regress-measure              # run all files and rewrite agreement.edn
AGREEMENT_FILES="geometry horology" bb pg-regress-probe
```

Full runs take roughly an hour on the development machine and run outside
per-PR CI. The gate flags a file that loses more than its recorded tolerance,
loses full expected-line coverage, or stops being measurable. A flagged drop
needs investigation; a higher score alone does not establish correctness.

For a direct upstream run with prerequisites in one disposable database:

```bash
bb pg-regress-with-fixtures date time timetz timestamp timestamptz interval horology
```

Keep the per-file manifest as the source of scores instead of maintaining a
second table that can drift. Regression artifacts under `.internal/pg-regress/`
contain the actual output and diffs needed to investigate a discrepancy.
