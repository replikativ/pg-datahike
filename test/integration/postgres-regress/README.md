# PostgreSQL regression baseline

This harness runs PostgreSQL's own `pg_regress` driver and unmodified
regression SQL against an existing pg-datahike server. It is a compatibility
baseline and triage tool, not an all-green gate.

Start pg-datahike on port 15432, then run one or more named tests:

```bash
bb pg-regress jsonb
bb pg-regress jsonb expressions
```

The default PostgreSQL source checkout is `../postgres`. Override paths and
the target endpoint when needed:

```bash
POSTGRES_SOURCE=/path/to/postgres \
PG_REGRESS_PORT=15436 \
PG_REGRESS_DB=datahike \
bb pg-regress jsonb
```

For independent discovery runs against a server configured with SQL database
provisioning, create and remove a uniquely named database automatically:

```bash
PG_REGRESS_ISOLATE=1 PG_REGRESS_PORT=15436 bb pg-regress int2 int4 int8 numeric bit
```

Only the harness-generated `pgdh_regress_*` database is removed. The database
named by `PG_REGRESS_DB` is used as the administrative connection and is never
dropped. Isolation fails early when the target has no `CREATE DATABASE` hook,
instead of silently reusing contaminated state.

The script uses PostgreSQL 17's installed `pg_regress` and `psql` by default.
Override them with `PG_REGRESS_BIN`, `PG_REGRESS_BINDIR`, or
`PG_REGRESS_MAJOR`.

The compatibility campaign is machine-readable in `campaign.edn`. List its
waves and current admission modes, or run a whole wave (optionally restricted
to one mode), with:

```bash
bb pg-regress-wave
bb pg-regress-wave 1
bb pg-regress-wave 1 discovery
```

`:unmeasured` means the upstream file has not yet been triaged on a clean
fixture, `:discovery` means it is continuously useful but has classified
prerequisites or differences, and `:strict` means its complete normalized API
output is a gate. Promote coherent application-facing statement groups into
focused differential tests before marking a dependency-heavy file strict.
Such admitted statement groups are recorded as `:strict-slices` with their
exact upstream line range and executable Clojure test var; campaign validation
fails if either provenance or gate goes stale.

Tests that consume relations created by another upstream file declare it with
`:requires`. The wave runner schedules each prerequisite once, before its first
consumer. For example, selecting the integer suites also runs PostgreSQL's
unmodified `test_setup.sql`, which creates and populates their shared tables.

Artifacts are written below `.internal/pg-regress/`: PostgreSQL's complete
result output, unified diff, and summary. A normal mismatch (`pg_regress`
status 1) is reported but does not fail the task. Harness failures remain
fatal. Set `PG_REGRESS_STRICT=1` when an admitted test is expected to be fully
green and should gate on any diff.

### Relational fixture bootstrap

PostgreSQL's `test_setup.sql` loads `onek` and `tenk` with server-side
`COPY FROM '/path'` and clones them with CTAS. Enabling arbitrary server-side
file reads merely for the suite would be unsafe. For relational tests, use a
fresh database and load the API fixtures through psql's client-side COPY:

```bash
PG_REGRESS_DB=regress_api bb pg-regress test_setup
PG_REGRESS_DB=regress_api bb pg-regress-bootstrap
PG_REGRESS_DB=regress_api bb pg-regress case subselect union join aggregates
```

The bootstrap creates `onek2`/`tenk2` when `test_setup` could not and streams
the unmodified upstream data into all four tables. It is intentionally a
one-shot operation: rerunning it appends the fixture rows again, so use a fresh
regression database for each independent baseline.

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
- internal-looking signatures such as JVM cast/nil failures, unknown Datalog
  variables, and lost connections.

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
