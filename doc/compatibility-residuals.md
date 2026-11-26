# PostgreSQL and application compatibility residuals

Evidence is anchored to the pg-datahike 0.1.431 release (PR #421) unless
a narrower test or older application run is identified. A green gate
checks its listed inputs and allowances. Coverage includes raw upstream
discovery, strict SQL assertions, client tests, and actual application
runs.

## PostgreSQL 18.6

The pinned source is REL_18_6. The inventory classifies 231 scheduled
files. The campaign has 90 files, including 12 whole-file strict gates
and 54 strict slices at 0.1.431. The frozen 164-file discovery result
predates the current server and uses a different adapter build and
launcher. Fresh raw results need the tested revision, fixture receipt,
and timeout profile for each file.

The targeted rerun on #421 recorded the following raw diagnostic counts
against the preceding schema batch. A count can fall when a fix removes
follow-on errors after an aborted transaction. Equal counts can still
accompany different rows or diagnostics.

| File | PostgreSQL 18.6 | Previous adapter | #421 |
|---|---:|---:|---:|
| identity | 52 | 133 | 125 |
| timetz | 10 | 14 | 10 |
| timestamptz | 34 | 60 | 50 |

The major migration residuals include partition attachment, ALTER DOMAIN,
and views containing nested schema-qualified joins. Identity USER
override, default sequence parameter behavior, and qualified trigger
restoration have smaller independent SQL surfaces. The query residuals
include historical time-zone parsing, view-definition rendering,
specialized index operator classes, and OLD/NEW aliases in RETURNING.
[Campaign and SQL findings](../test/integration/postgres-regress/pg18/residuals.edn)

## Client and application evidence

| Surface | Verified scope | Gaps and limits to the evidence |
|---|---|---|
| asyncpg v0.30.0 | Eleven upstream modules. Of 203 selected owning test IDs, 118 pass cleanly, 53 are expected failures and 32 skip upstream. Four additional IDs are explicitly deselected. | See the cause breakdown below. Only the admitted IDs participate in the gate. |
| pgJDBC REL42.7.5 | Eight classes, 275 passing cases and one upstream skip. Covers text and binary results, prep, batches, and selected metadata. | Recheck ResultSetMetaData and DatabaseMetaData classes after the identity and domain fixes. |
| node-postgres 8.14.1 | 18 of the 22 selected files passed on #421, with four known-gap files. The two repeat unexpected passes are now strict. | The four file-level allowances can conceal other failures in those files. Row error metadata must preserve logical temporary table names. |
| Hibernate 6.4.4.Final | 13 app tests with the stock PostgreSQL dialect and pgJDBC 42.7.3 in simple-query mode. | The suite leaves extended-query Hibernate behavior unmeasured. |
| SQLAlchemy and psycopg2 | 16 workflows with the custom Datahike dialect. | The custom dialect overrides version, capability and reflection paths. The CI Python dependencies are unpinned. Stock dialect startup and reflection need a separate check. |
| Pagila and Chinook | Pagila checks COPY and row counts across 21 tables. Chinook has a separate strict per-row bidirectional gate. | Pagila tolerates schema errors. A local #421 profile disabled index backfill on its new database, causing 21 key and 36 downstream foreign-key errors. The precreated hosted database and standalone launcher enable backfill. The revised test template uses the same backfill setting. Qualified trigger references, nested joins in views, and partition attachment produced independent errors. |
| Odoo 19 | The archived manual base smoke passed 11 TestORM cases. The archived accounting boot reached module init and failed on the tax-tag UPDATE. | The current base runner accepts a floor of nine and reports the Odoo test-process exit code. The accounting query uses FOR-only substring. Posted invoice numbering relies on a partial UNIQUE index and needs its own concurrency test. |
| Metabase 0.60.2.4 | The archived manual app run completed initial sync and a native query. A #421 focused replay executed all 17 captured SQL statements against the seed, with 16 returning without error. | The current per-commit assertion checks statement routing. The seventeenth execution failed on missing pg_type.typtypmod in pgJDBC getColumns. FK discovery returned no rows in the replay. The optional MBQL shell probes now fail on HTTP errors and missing result sets. Exact result-value assertions still need to be added. |

The archived full-application evidence is older than the client and SQL
receipts. The published build for any new live run, and its exact seed,
driver and launch profile, should accompany each result.
[Release gates](beta-exit.md)

### Candidate application checks

The sequence migration probes matched stock PostgreSQL 18.6, and the
extracted Odoo tax-tag UPDATE returned the intended rows. The
candidate also replayed all 17 captured Metabase statements. The JDBC
column query returned five rows and the added focused JDBC test verified
domain precision and scale. The Metabase replay used the captured seed
and SQL.

A fresh Pagila restore using the production backfill setting matched row
counts across all 21 tables. Its 42 statement errors, compared with 99
under the previous local fixture, still include qualified trigger
references, nested view joins, partition attachment and other DDL.
Backfill accounts for the 57 eliminated fixture errors.

Qualified trigger restoration remains open. Binding both the table and
function qualifiers requires schema-aware routine resolution. In the
current registry, a bare function name cannot distinguish that reference
from a same-named routine declared in another schema.

A fresh Odoo 19 base run first needed the conventional postgres
administrative database on the private test endpoint. With that fixture
in place, Odoo stopped during base SQL initialization on ON DELETE SET
NULL (SQLSTATE 0A000). Its TestORM cases were not reached. The prior
11-case report is historical. The manual harness now provisions that
database, and referential SET NULL behavior remains a server gap.

### asyncpg expected failures by owning ID

| Area | IDs | Observed causes |
|---|---:|---|
| Codecs | 25 | Binary interval and record output, composite and domain identity, enum arrays, ranges, extension setup, and isolated type cases. |
| Cache invalidation | 9 | Seven stop on an initial query before the ALTER. Two stop on the pool fixture's hard-coded postgres database. |
| Prepare | 6 | Decimal typing, EXPLAIN JSON, record decoding, named statement visibility from SQL, and invalidation ErrorResponse metadata. |
| Connect | 4 | External connection settings leaked into the upstream native primary/standby test fixture. |
| Introspection | 4 | Two extension fixtures, large inherited catalogs, and cache-bust parameter typing. |
| Cursor, exceptions, execute | 5 | SQL MOVE against wire portals, missing error position, blank SQL function body, and abrupt-close cancellation. |

The exact manifest fell from 56 owning IDs at #369 to 53 at #414 and
remained at 53 through #421. Pytest's event totals also count subtests.
Pool lifecycle and COPY are the largest unselected application-facing
modules, with 50 and 24 source-defined methods. Authentication and TLS
need separately provisioned evidence.
[Manifest and runner](../test/integration/asyncpg/expected-failures.txt)

## Next verification work

1. Implement the referential SET NULL path, including the constraint
   metadata Odoo inspects. Then rerun Odoo base and accounting boot.
   Check invoice numbering separately with its partial uniqueness and
   concurrency workload.
2. Assert admitted constraints, view results and trigger effects alongside
   Pagila COPY counts. Repeat Metabase initial sync on a named release and
   validate JDBC metadata and representative MBQL values.
3. Close asyncpg's SQL MOVE bridge and cached-plan error metadata gaps with
   the upstream cases. Correct binary interval and record handling before
   expanding codec and pool admission.
4. Refresh deferred pgJDBC metadata classes, replace node-postgres file
   allowances with exact tests, and add a stock SQLAlchemy reflection probe.
