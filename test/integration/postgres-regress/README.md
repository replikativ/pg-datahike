# PostgreSQL regression baseline

This harness runs PostgreSQL's own `pg_regress` driver and unmodified
regression SQL against an existing pg-datahike server. It is a compatibility
baseline and triage tool, not an all-green gate.

Start the pg-datahike build you intend to measure on port 15432, materialize
the pinned source, then run one or more named tests:

```bash
bb pg-regress-setup
bb pg-regress jsonb
bb pg-regress jsonb expressions
```

`PG_REGRESS_PROFILE` selects the reference source and client-tool major together:

| Profile | Source release | Available operations |
|---|---|---|
| `17` (default) | `REL_17_7` | Named tests, fixture runs, audited waves and agreement |
| `18` | `REL_18_6` | Named/fixture discovery, audited inventory and unmeasured waves |

For example, with PostgreSQL 18's tools installed:

```bash
PG_REGRESS_PROFILE=18 bb pg-regress-setup
PG_REGRESS_PROFILE=18 bb pg-regress int4
```

This selects the test reference; it does not change pg-datahike's advertised
version or runtime catalog. Profile 18 has a separate source-audited inventory,
with 231 scheduled tests and 233 SQL files (including two unscheduled files).
Its 80 campaign files start unmeasured; it has no strict slices, admitted routine
evidence or agreement scores. The all-SQL scope selects 164 application-facing
files and excludes 69 files with explicit scope reasons. Inventory counts
describe source dispositions, not executed or passing tests.

```bash
PG_REGRESS_PROFILE=18 bb pg-regress-wave inventory
```

The inventory fingerprints the SQL, schedule and all 265 expected output files,
including platform variants. The source schedule documents shared setup and
ordering requirements; those do not establish that every file is ready to run
in isolation. Native regression helpers and encoding/file fixtures remain
explicit fixture boundaries. A full agreement measurement requires validation
of those prerequisites; the empty profile18 baseline is not a release gate.

Sources default to `.internal/postgres-REL_17_7` or
`.internal/postgres-REL_18_6`. A missing pinned checkout is an error; the harness
does not fall back to a moving `../postgres` checkout. Override the source and
target endpoint when needed:

```bash
POSTGRES_SOURCE=/path/to/pinned-postgres \
PG_REGRESS_PORT=15436 \
PG_REGRESS_DB=datahike \
bb pg-regress jsonb
```

`POSTGRES_SOURCE` names the checkout; the legacy `PG_REGRESS_SRC` names its
`src/test/regress` directory. If both are supplied they must agree. Connection
credentials use standard libpq environment variables such as `PGPASSWORD` or
`PGPASSFILE`; connection URIs/conninfo are not accepted as `PG_REGRESS_DB`.

For independent discovery runs against a server configured with SQL database
provisioning, create and remove a uniquely named database automatically:

```bash
PG_REGRESS_ISOLATE=1 PG_REGRESS_PORT=15436 bb pg-regress int2 int4 int8 numeric bit
```

Only the harness-generated `pgdh_regress_*` database is removed. The database
named by `PG_REGRESS_DB` is used as the administrative connection and is never
dropped. Isolation fails early when the target has no `CREATE DATABASE` hook,
instead of silently reusing contaminated state.

The selected profile defaults to `/usr/lib/postgresql/MAJOR/bin/psql` and the
matching installed `pg_regress`. `PG_REGRESS_BIN` and `PG_REGRESS_BINDIR` can
point at another installation of that major. Tool versions and an explicitly
supplied `PG_REGRESS_MAJOR` must agree with the profile.

Each run retains raw output and a `reference.edn` receipt under
`.internal/pg-regress/REL_*/RUN/`, or the empty directory explicitly supplied as
`PG_REGRESS_OUTPUT`. Receipts identify the source commit/input hashes, tool
versions, preflight-session server version/settings and completion status. The
preflight settings describe that connection, not every regression session; the
upstream driver also sets session defaults. Receipts record operator timeout
settings, hash `PGOPTIONS` without storing its contents, and fingerprint the
runner harness files. The runner checkout is identified separately: the external server's
code revision is **unverified**, and its advertised version does not establish
which build is running. Start/restart the intended server build before measuring.

Agreement commands also require an already-running endpoint and never start,
stop or recycle a server. Results stay in a versioned run directory; an optional
`AGREEMENT_KEEP` must name an empty collection directory. A probe can select files:

```bash
AGREEMENT_FILES="regproc type_sanity" bb pg-regress-probe
```

Client observations and database cleanup have finite process and statement
limits. Agreement file timeouts also force termination after a short grace period.
The JVM comparison has a 512 MiB heap limit and a 30-minute process deadline;
set `AGREEMENT_COMPARE_TIMEOUT` (for example, `10m`) to change the deadline.
Completed per-file comparisons are checkpointed using hashes of both inputs.
To resume comparison after interruption without rerunning SQL, use the completed
collection directory and the same profile/source settings:

```bash
bash test/integration/postgres-regress/agreement.sh compare probe /path/to/collection
```

The same comparison deadline and heap limit apply when resuming. Checkpoints do not replace a fresh SQL collection for a
new build. The runner promotes a measured baseline only after its comparison
process finishes successfully. Only complete results owned by that run are collected. Timeouts, connection loss,
missing outputs, changed inputs and cross-version baselines fail the measurement;
older `.out` files are not reused. `pg-regress-measure` rewrites the primary
baseline only after a complete authoritative run. Agreement remains a canonical
expected-line score, distinct from `pg_regress`'s whole-file/expected-variant result.

Run the filesystem/subprocess harness tests without database servers:

```bash
bb pg-regress-harness-test
```

The compatibility campaign is machine-readable in `campaign.edn`. List its
waves and current admission modes, or run a whole wave (optionally restricted
to one mode), with:

```bash
bb pg-regress-wave
bb pg-regress-wave inventory
bb pg-regress-wave 1
bb pg-regress-wave 1 discovery
bb pg-regress-wave 2 strict
```

The inventory accounts for every test in PostgreSQL's pinned
`parallel_schedule`. Tests in `campaign.edn` are being measured or already
gate focused behavior. The remaining scheduled files are grouped in
`scope.edn` as either application-facing backlog or deliberately out of scope,
with a reason for each group. Campaign validation fails when an upstream test
is omitted, named twice, assigned to both files, or no longer exists in the
pinned schedule. This makes the denominator explicit without pretending that
PostgreSQL storage, planner, replication, or server-administration internals
are pg-datahike release criteria.

The campaign is pinned to the PostgreSQL tag recorded as `:postgres-ref` in
`campaign.edn` and the selected profile. `POSTGRES_SOURCE` must point at that
release commit with clean regression SQL, expected outputs, schedules and fixture
inputs. For deliberate exploratory named runs against changed inputs, set
`PG_REGRESS_ALLOW_UNPINNED=1`. Such runs are labeled non-authoritative and cannot
rewrite/compare agreement baselines or run strict slices.

Create and verify the pinned checkout under the ignored `.internal/` directory
without modifying an existing `../postgres` tree:

```bash
bb pg-regress-setup
bb pg-regress-wave inventory
```

Pass a target directory to `bb pg-regress-setup`, or set `POSTGRES_SOURCE`, to
put the checkout elsewhere. The setup command refuses to rewrite an existing
checkout at the wrong revision.

`:unmeasured` means the upstream file has not yet been triaged on a clean
fixture, `:discovery` means it is continuously useful but has classified
prerequisites or differences, and `:strict` means its complete normalized API
output must match whenever that mode is run. Promote coherent
application-facing statement groups into focused differential tests before
marking a dependency-heavy file strict.
The inventory's classified-file percentage combines `:discovery` and
`:strict`: it measures how much of the application-facing schedule has been
run and explained, not how much PostgreSQL pg-datahike implements.
Such admitted statement groups are recorded as `:strict-slices` with their
exact upstream line range and executable Clojure test var; campaign validation
fails if either provenance or gate goes stale.

The routine/trigger expansion has a second validated ledger in
`routine-trigger-capabilities.edn`. It records category dependencies and exact
PostgreSQL 17 evidence ranges without treating an entire regression file as a
coverage claim. A range becomes `:admitted` only when it names a live focused
gate under `test/datahike/test/`, whose `deftest` explicitly declares
`^{:postgres-evidence :evidence-id}` metadata. The implementation invariants
and product boundary are documented in
[`doc/routines-and-triggers.md`](../../../doc/routines-and-triggers.md).

Tests that consume relations created by another upstream file declare it with
`:requires`. The wave runner schedules each prerequisite once, before its first
consumer. For example, selecting the integer suites also runs PostgreSQL's
unmodified `test_setup.sql`, then safely populates its shared tables through
the client protocol. The complete setup and selected tests share one
automatically removed database.

Artifacts are written below `.internal/pg-regress/`: PostgreSQL's complete
result output, unified diff, console log, and a machine-readable `summary.tsv`.
A normal mismatch (`pg_regress` status 1) is reported but does not fail the
task. Harness failures remain fatal. Bound an exploratory invocation with, for
example, `PG_REGRESS_TIMEOUT=15m`; status 124 then identifies the incomplete
run, while `pg_regress.log` preserves its last progress line. The runner also
passes a 10-second server-side `statement_timeout` through PGOPTIONS by
default. This bounds each statement independently and lets the file continue
after a pathological query; set `PG_REGRESS_STATEMENT_TIMEOUT` to another
duration or to `0` to disable it. Keep the invocation timeout as the outer
harness safety net.

Set `PG_REGRESS_STRICT=1` when an admitted test is expected to match
raw psql output, including source-position presentation. Campaign tests in
`:strict` mode instead gate on the documented API-normalized comparison, which
retains messages, DETAIL/HINT fields, rows, labels, types, and formatting while
omitting only PostgreSQL's rendered `LINE`/caret excerpts.

### Relational fixture bootstrap

PostgreSQL's `test_setup.sql` loads `onek` and `tenk` with server-side
`COPY FROM '/path'` and clones them with CTAS. Enabling arbitrary server-side
file reads merely for the suite would be unsafe. For relational tests, use the
fixture runner, which creates a fresh database, runs `test_setup`, loads through
psql's client-side COPY, runs the selected tests, and removes the database even
when a test fails:

```bash
PG_REGRESS_DB=datahike bb pg-regress-with-fixtures case subselect union join aggregates
```

`PG_REGRESS_DB` names the administrative database and is never removed. Only
the generated `pgdh_regress_fixture_*` database is dropped. The server must be
configured with SQL database provisioning, just as for
`PG_REGRESS_ISOLATE=1`. Campaign waves choose this runner automatically when a
selected test declares `test_setup` as a prerequisite. The bootstrap verifies
the exact row counts instead of accepting partial writes, and its output is
retained as `bootstrap.log` beside the setup and target artifacts.

The fixture restores `onek`, `tenk`, and `person` through client-side COPY. For
PostgreSQL's inherited `emp`, `student`, and `stud_emp` tables it loads the
declared scalar columns from the original rows, which makes independent
aggregate and DML behavior measurable without claiming inheritance support.
The `road` family remains unavailable because PostgreSQL `path` and inherited
column layouts are not implemented; campaign diffs that touch those surfaces
remain classified compatibility gaps. The lower-level `pg-regress-bootstrap`
task remains available for diagnosing fixture loading in a caller-managed
fresh database.

## Reading the baseline

The raw diff includes harmless differences such as shorter error diagnostics,
alongside unsupported features and genuine wrong answers. The runner therefore
also prints:

- total diff lines as a coarse trend;
- per-test expected/target error counts and their coarse delta, plus
  transaction-cascade and internal-failure counts;
- `api-match`, which compares complete output after removing only PostgreSQL's
  `LINE n:` source excerpt and caret presentation from errors;
- the most frequent target-side errors;
- internal-looking signatures such as JVM cast/nil and protocol-dispatch
  failures, malformed generated Datalog, raw numeric-parser errors, unknown
  Datalog variables, and lost connections.

An `aborted` count is not a count of independent defects. One unexpected error
inside an explicit transaction can turn every following statement into
`current transaction is aborted`; fix or classify the first unexpected error
before using the remainder of that test as coverage data.

The error delta is also only a triage signal: matching counts do not prove that
the same statements failed, and one missing expected error can cancel one extra
target error. Use the unified diff to establish behavior before promoting a
slice to a gate.

`api-match=yes` is stronger: all commands, errors, rows, column labels and
psql-rendered values agree after the narrow source-position normalization. It
does not claim wire-level ErrorResponse position fields are implemented, but
it is suitable for admitting an SQL API behavior slice.

## Compatibility campaign

Work through upstream tests in dependency-preserving slices rather than passing
the entire schedule at once:

1. `test_setup` plus scalar types and `expressions` — literal typing, operator
   resolution, casts, NULL/error semantics, and result OIDs.
2. Core DDL/DML — `create_table`, `constraints`, `insert`, `update`, `delete`,
   `copy`, and `transactions`.
3. Relational queries — `select*`, `join`, `subselect`, `union`, aggregates,
   grouping, arrays, windows, and CTEs.
4. Application-facing types — date/time, UUID, enum/domain, JSON/JSONB, and
   eventually full-text and vector extensions.
5. Protocol/catalog surfaces — prepared statements, portals, psql, and the
   catalog queries exercised by real drivers and ORMs.

PostgreSQL's suite also tests PostgreSQL's own storage engines, planner nodes,
server administration, procedural languages, replication, and extension APIs.
Those are useful for discovering parser or catalog assumptions, but are not a
release criterion for a Datahike-backed engine. Record them as deliberately
out of scope instead of weakening errors or adding no-op implementations merely
to reduce the diff. The release-facing target is: no internal failures, no
silent wrong answers in the supported SQL surface, explicit SQLSTATE-bearing
errors for unsupported features, and strict upstream slices for behavior we
have admitted.

Do not reduce the diff count by merely matching diagnostic wording. Promote
high-value behavior into focused unit or SQLLogic tests, and treat silent wrong
answers and internal failures ahead of explicitly unsupported surface.

The JDBC differential runner also compares update counts, result-set labels
and types, SQLSTATE, and stable structured error fields. Equal row text or the
fact that both engines failed is not sufficient evidence of compatibility.

## PostgreSQL source map

Use PostgreSQL's implementation and catalogs to establish semantics before
adding compatibility behavior. This map gives the usual starting points; keep
specific issue investigations and changing regression results in their GitHub
issues or pull requests rather than turning this document into a historical
ledger.

| Boundary | PostgreSQL authority | Upstream regression corpus | pg-datahike coverage |
|---|---|---|---|
| Expression parsing, parameter typing, and lowering | `src/backend/parser/parse_expr.c`, `parse_oper.c`, `parse_coerce.c`; `src/include/catalog/pg_proc.dat`, `pg_operator.dat` | `expressions.sql`, focused statements in type suites | expression/OID tests and extended-query pgjdbc tests |
| Set-returning functions | overloads in `pg_proc.dat`; integer implementations in `src/backend/utils/adt/int.c` | `rangefuncs.sql` | SRF, lateral-SRF, and extended-query tests |
| Type lookup and casts | `src/backend/parser/parse_type.c`, `parse_coerce.c`; `src/include/catalog/pg_type.dat` | per-type suites and `type_sanity.sql` | type-resolution, cast, enum/domain, and DDL-fidelity tests |
| Arrays and array input | `src/backend/utils/adt/arrayfuncs.c` (`array_in`, `ReadArrayStr`) | `arrays.sql` plus each element type's suite | array codec, SQL array, and array-column tests |
| Catalog row descriptions | catalog headers such as `pg_type.h`, `pg_attribute.h`, `pg_class.h` | `type_sanity.sql` and driver metadata queries | catalog tests plus pgjdbc/ORM probes |
| Relation resolution and errors | `src/backend/parser/parse_relation.c` | broadly exercised across the suite | unknown-table/column and catalog tests |
| `EXPLAIN` grammar/API | `src/backend/parser/gram.y`, `src/backend/commands/explain.c` | `explain.sql` | accepted-option and unsupported-feature tests |
| `money` | `src/backend/utils/adt/cash.c`; money entries in `pg_type.dat`, `pg_cast.dat`, and `pg_operator.dat` | `money.sql` | strict comparison, assignment-input, division-rounding, and arithmetic-overflow slices; full locale-rendered suite remains discovery |
