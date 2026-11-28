# PostgreSQL and application compatibility residuals

Evidence is anchored to the pg-datahike 0.1.431 release (PR #421) unless
a narrower test or older application run is identified. A green gate
checks its listed inputs and allowances. Coverage includes raw upstream
discovery, strict SQL assertions, client tests, and actual application
runs.

## PostgreSQL 18.6

The pinned source is REL_18_6. The inventory classifies 231 scheduled
files. The campaign has 91 files, including 12 whole-file strict gates
and 64 strict slices after the expression-index batch. The
frozen 164-file discovery result uses an older adapter build and launcher.
Fresh raw results need the tested revision, fixture receipt, and timeout
profile for each file.

The 0.1.431 discovery refresh attempted all 164 files and collected 158
complete outputs. Of the 157 with a comparable frozen result,
normalized matches rose from 11 to 13. The new matches were `enum.sql`
and `time.sql`. Normalization removes diagnostic source positions.
`alter_table`, `database`, `generated_stored`, `inherit`, `join`, and
`sqljson` remained incomplete.

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
| Metabase 0.60.2.4 | The archived manual app run completed initial sync and a native query. A #421 focused replay executed all 17 captured SQL statements against the seed, with 16 returning without error. | The current per-commit assertion checks statement routing. The seventeenth execution failed on missing pg_type.typtypmod in pgJDBC getColumns. The captured seed uses a Datahike-native reference and declares no SQL FK. A separate fixture with three declared SQL FKs returned the same three rows as stock PostgreSQL. The optional MBQL shell probes now fail on HTTP errors and missing result sets. Exact result-value assertions still need to be added. |

The archived full-application evidence is older than the client and SQL
receipts. The published build for any new live run, and its exact seed,
driver and launch profile, should accompany each result.
[Release gates](beta-exit.md)

### Application checks for PR #422

The sequence migration probes matched stock PostgreSQL 18.6, and the
extracted Odoo tax-tag UPDATE returned the intended rows. The
candidate also replayed all 17 captured Metabase statements. The JDBC
column query returned five rows and the added focused JDBC test verified
domain precision and scale. The Metabase replay used the captured seed
and SQL. The captured FK query also has an assertion for a declared SQL
foreign key.

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
database.

The SET NULL follow-on passed database initialization. The next boot
failure was the `information_schema.constraint_column_usage` lookup in
`ir_logging.init` (SQLSTATE 42P01). The added view reports the referenced
table and columns of a foreign key, with the constraint's own schema.
Its key names follow the existing `pg_constraint` metadata, which still
synthesizes the names of some explicit PRIMARY KEY and UNIQUE constraints.

With both changes, the live boot advanced to converting
`res_users.login` from `varchar(64)` to unconstrained `varchar`. The ALTER
failed on the column's dependencies (SQLSTATE 0A000), before TestORM.

The PostgreSQL fixture verifies SET NULL on self-references and
cross-table references, child-row preservation, rollback and a NOT NULL
failure. The DELETE guard can return 40001 if a concurrent write
changes the database while the referential action is being prepared.
The targeted `foreign_key.sql` discovery run went from 857 adapter
diagnostics on 0.1.431 to 832, with 198 expected from stock and zero
internal failures. Partitioning, deferred constraints and several
referential action variants still affect the full file. ALTER restores
continue to admit existing ON UPDATE CASCADE declarations with
restrictive parent-key updates.

### Odoo login widening and device views

The adapter now widens a bounded varchar column by updating its typmod.
The Odoo login ALTER keeps the original column and unique key, accepts
longer values, and preserves the prior width in historical reads. The
strict PostgreSQL suite gained the `alter_table.sql` slice that widens a
varchar primary key in the transaction that creates it. A prepared JDBC
statement verifies that it sees the new descriptor.

Odoo's parenthesized `res_device` SELECT now creates a view. The captured
query produces the same rows as PostgreSQL 18.6 on a fixture covering
revocation, null-safe grouping, and tied activity times.

A fresh Odoo 19 base boot at commit `007e86391e14` passed both statements
and reached model-field reflection before TestORM. Odoo declares
`UNIQUE (model, name)` through ALTER TABLE on `ir_model_fields`. A later
`INSERT ... ON CONFLICT (model, name) DO UPDATE` failed with SQLSTATE
42P10. Odoo also caught two nonfatal errors creating device-log partial
indexes with `WHERE revoked IS NOT TRUE`.

### Composite unique constraints and conflict inference

The strict campaign includes the composite `ADD UNIQUE` sequence from
`alter_table.sql` (706–718) and the key inference sequence from
`insert_conflict.sql` (133–141). The Odoo field reflection fixture uses
a named constraint, adds it after writes in the same transaction, then
verifies that its upsert updates the original row. The backing index
also rejects duplicate writes and the ALTER validates existing rows.
Catalog and dump probes cover the constraint name, column order,
index ownership, and rollback. A PostgreSQL 18.6 restore of the adapter's
`pg_dump` output retained upsert inference and duplicate rejection.

The live Odoo 19 bootstrap at `007e86391e14` completed field and selection
reflection. It stopped before TestORM on a partial index for
`ir_ui_view.key` with `WHERE "key" IS NOT NULL` (42601). Odoo also caught
two device-log partial-index syntax errors and 21 inherited-column
`SET NOT NULL` errors.

The partial-index repro also rejects an equality predicate in CREATE
INDEX. The same predicates in SELECT return the expected rows. Compare
the app's index to `aggregates.sql` at line 427, and the inherited
nullability changes to `inherit.sql` starting at line 827.

### Ordinary partial indexes

The Odoo `ir_ui_view` non-null predicate and the two device-log boolean
predicates now round-trip through index reflection. The source-linked
gates cover the inherited-table index in `aggregates.sql` (421–435) and
the distinct equality predicates in `create_index.sql` (1440–1441).
They verify result rows as well as predicate text. Stock PostgreSQL and
pg-datahike both restored the tested predicates from the adapter's
`pg_dump` output.

This batch supports ordinary non-unique B-tree declarations on column
keys. The adapter stores the validated predicate as catalog metadata,
which follows column renames and transactional drops. SQL filtering
uses Datahike's general access paths. Predicate uniqueness, partial
index inference for ON CONFLICT, and expression index keys remain
separate coverage gaps.

The live Odoo 19 bootstrap at `007e86391e14` created the device-log,
view-key, and company-registry partial indexes. Its next fatal statement
was a B-tree index on the expression `("barcode" IS NOT NULL)` for
`res_partner`, with `WHERE "barcode" IS NOT NULL` (42601). That precedes
TestORM. The 21 caught inherited-column `SET NOT NULL` errors recur.

### Ordinary expression index keys

The barcode case and the partial arithmetic case from `indexing.sql`
both preserve an expression key with zero in `indkey`, the expression
in `indexprs`, and the key's type in `pg_attribute`. The barcode query
distinguishes SQL NULL from JSON `null`. The `create_table.sql` slice
pairs expression-key dependencies with durable column positions after
earlier columns are dropped. The audited inventory admits the DDL
slice from `indexing.sql`. Its whole-file measurement remains open.

These gates cover declarative non-unique B-tree keys on ordinary
tables. SQL queries evaluate expressions through Datahike's general
access paths. Expression uniqueness, storage-backed expression
access paths, partition-index attachment, and wider expression syntax
remain separate coverage gaps.

The live Odoo 19 boot at `007e86391e14` created the barcode expression
index and proceeded to add foreign keys. The 21 caught inherited-column
`SET NOT NULL` errors still recur.

### Odoo foreign-key reflection and inherited primary keys

The wire executor can look up foreign keys by child relation name and
follow their relation OIDs to both sides of the key. It resolves the
names against the execution snapshot, including for prepared queries.
Unrestricted catalog scans and queries that also read user tables use
the existing catalog projection.

Odoo 19 at `007e86391e14` reached the FK from
`ir_act_window_group_rel.act_id` to `ir_act_window.id` in a fresh base
installation. It stopped with 42830 before TestORM. The target table was
created with `CREATE TABLE ir_act_window (PRIMARY KEY (id)) INHERITS (ir_actions)`. Its child-local
primary key must be available as a foreign-key target. The trace also
recorded 87 reads of `information_schema.columns` by table name, taking
about 29 seconds together in this run.

The PostgreSQL 18.6 `alter_table.sql` statements at lines 2186–2187
cover preservation of the explicit name on an inline foreign key. The
adapter gate checks the name in `pg_constraint` and drops the constraint
by that name.

### Odoo external IDs and registry initialization

Odoo 19 at `007e86391e14` creates the external-ID table, inserts bootstrap rows,
and creates composite indexes in one SQL transaction. With Datahike 0.8.1958
a full scan after commit found rows that indexed reads missed. The paired
`alter_table.sql` gate (2349–2351) checks that an equality lookup sees an
earlier insert after commit and a duplicate is rejected. The Odoo fixture
checks both composite lookups and a subsequent upsert.

With the Datahike #1198 index fix, a separate connection saw
`base.group_user` and `base.module_category_accounting` after bootstrap.
The module-list update passed. Ten of the eleven base ORM cases passed in
that run. The remaining case, `test_m2m_store_trigger`, reached a group
membership query combining five correlated EXISTS predicates. Its captured
SQL now has a focused PostgreSQL 18.6 comparison, including separate tests
for NULL and empty membership sets. With the membership change, a fresh base installation
with optional module auto-installation disabled exited successfully and
passed all 11 ORM cases.

The same installation reached the auto-installed web module. Updating
`res_company.paperformat_id` then failed with SQLSTATE 40001 during
transaction rebase. The logged transaction numbers were 536870964 and
536871995. A single-session reproduction failed after creating a table with a
private serial sequence and advancing a committed sequence. The focused
gate verifies the update commits and genuine overlapping writes still fail.
Stock PostgreSQL 18.6 completed initialization of all 14 modules.

### Odoo catalog reflection

Odoo's column, constraint and relation-existence queries can select the
catalog rows for a named relation at execution time. SQL still applies the
namespace predicate against the statement snapshot. Bound table-name
parameters and broader catalog scans use the general execution path.
A named composite unique constraint now returns its declared name and
definition. Older tables whose original name was discarded retain the
generated name. Column introspection also returns pg-datahike's synthetic
`db_id` column.

For `pg_attribute` nullability checks, a join on the named table's OID can
read its live columns directly. Odoo also filters by column name and
formatted type. Dropped columns disappear from this lookup and reappear
after savepoint rollback. Queries requesting dropped columns, indexes or
views still use the full catalog. The prepared-query gate checks repeated
`SET NOT NULL` against PostgreSQL 18.6's `constraints.sql` lines 639–641.

The [64-table benchmark](../bench/catalog-reads.py) exercises all four
queries on pg-datahike and PostgreSQL. The transaction gate also checks that
aborting a table creation removes its `pg_class` row and restores deleted
rows, following PostgreSQL 18.6's `transactions.sql` lines 21–36.

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

1. Continue Odoo base and accounting checks past index setup. Pair
   inherited nullability with `inherit.sql` and verify child-local
   constraints and parent-to-child propagation. Verify posted invoice
   numbering with its partial-uniqueness and concurrency workload.
2. Assert admitted constraints, view results and trigger effects alongside
   Pagila COPY counts. Repeat Metabase initial sync on a named release and
   validate JDBC metadata and representative MBQL values.
3. Close asyncpg's SQL MOVE bridge and cached-plan error metadata gaps with
   the upstream cases. Correct binary interval and record handling before
   expanding codec and pool admission.
4. Refresh deferred pgJDBC metadata classes, replace node-postgres file
   allowances with exact tests, and add a stock SQLAlchemy reflection probe.
