# Integration testing model

pg-datahike is tested at four complementary boundaries. The jobs in layers
1–3 run for every pull request; suites with known upstream gaps use explicit
manifests so only a new regression fails the build. Layer 4 is a local
discovery and admission workflow.

## Layer 1 — unit (per-commit)

```
bb test
bb sqllogictest
```

A current full run reports 1,604 tests / 6,808 assertions and the SQLLogic
runner reports 61 assertions. Treat the runner output, rather than these
snapshot counts, as authoritative as coverage grows.

Run on every PR + commit. Covered by the `unittest` and `sqllogictest`
jobs in the main workflow.

Exercises the translator, handler dispatch, classifier, rewriter, shape
matcher, embedded pgwire/JDBC behavior, catalog goldens, and SQL-conformance
surface. The unit job also has a real PostgreSQL 16 sidecar for cross-engine
round-trip tests.

## Layer 2 — wire + ORM conformance (per-commit)

Three application-level jobs run against a live pgwire server on :15432:

```
pgjdbc-conformance       80 ResultSetTest cases — ~6 min warm daemon
hibernate-app-conformance 14 end-to-end tests — ~2 min
sqlalchemy-conformance    16 tests across 7 phases — <30 s
```

`pgjdbc-conformance` is the canonical wire-protocol regression catch-net —
pgjdbc exercises Simple Query, Extended Query (Parse/Bind/Describe/Execute),
RowDescription, ErrorResponse, and parameter inference end-to-end.

`hibernate-app-conformance` runs a custom `DatahikeHibernateTest`
(under `test/integration/hibernate-app/`) that exercises Hibernate 6
through all six phases: DDL boot (hbm2ddl create), basic CRUD,
relationship mapping, HQL aggregates, transactions, and native SQL.

`sqlalchemy-conformance` runs `test/integration/test_sqlalchemy.py`
through SQLAlchemy 2.0 + psycopg2 + the custom `datahike_dialect.py` —
7 phases covering DDL, CRUD, relationships, aggregates, transactions
including ROLLBACK, raw SQL, and schema introspection.

All three gate `deploy`.

## Layer 3 — client and dump regression gates (per-commit)

| Harness | Boundary | Gate contract |
|---|---|---|
| asyncpg | Independent async wire implementation, codecs, prepared statements, transactions, and introspection | Every included test runs; a failure not in `expected-failures.txt` fails CI |
| node-postgres | Independent JavaScript wire implementation and common `pg` API behavior | Must-pass files gate CI; documented known-gap files run as XFAIL and report XPASS |
| pg_dump round-trip | PostgreSQL 16 default-format dump of Pagila restored through pgwire | Restore and data checks must complete without an unexpected failure |

The distinction between a green job and complete upstream compatibility is
important. asyncpg and node-postgres intentionally continue to execute known
unsupported cases. Their checked-in manifests keep those gaps visible while
making any new failure a per-commit regression. When an expected failure starts
passing, the harness reports it so the manifest can be tightened.

The asyncpg, node-postgres, and pg_dump round-trip jobs gate deployment.

Each harness follows the same shape:

```
  - bb prep                         ;; compile Java
  - start pgwire on :15432 (&)      ;; background
  - wait for :15432
  - <harness>/setup.sh              ;; download client, build venv/npm, ...
  - <harness>/run.sh                ;; run conformance tests against server
```

The `setup.sh` / `run.sh` scripts live under `test/integration/<harness>/`
and are shared between local dev (`cd test/integration/asyncpg && ./run.sh`)
and CI (just wrapped in a job).

## Cross-engine differential testing

`datahike.test.cross-engine` is a dev + triage tool that runs the
same `.test` file against two PG-wire endpoints over pgjdbc and
diffs the result sets. Use to isolate dialect-drift bugs from
execution-engine bugs.

```
# Requires a real Postgres on :5432 AND our pgwire on :15432
REFERENCE_URL=jdbc:postgresql://localhost:5432/test?user=pg \
TARGET_URL=jdbc:postgresql://localhost:15432/datahike?user=datahike \
  bb cross-engine test/sqllogictest/test_select.test
```

Output:
```
== test/sqllogictest/test_select.test
   passed= 24 failed= 3
   SQL: SELECT a, b FROM t1 WHERE a = 2
     only in ref:    [[2 "two"]]
     only in target: []
   ...
TOTAL passed= 24 failed= 3
```

Exit 0 when every query's result set matches (respecting the declared
`rowsort`/`valuesort`/`nosort` mode). Exit 1 otherwise.

Intentional non-matches (don't red-flag):
- tie-order in `nosort` ORDER BY
- error-message wording differences (only SQLSTATE is compared)
- data-type inference for untyped literals in SELECT projection

Not wired into CI — needs a running Postgres. Use locally during
feature development; copy surprising diffs into `sqllogictest/` as
new test cases once fixed.

## Layer 4 — PostgreSQL's upstream regression suite

`bb pg-regress` runs PostgreSQL's own `pg_regress` driver, SQL, and expected
output against an existing pg-datahike server. It uses `../postgres` and the
installed PostgreSQL 17 tools by default:

```
bb pg-regress jsonb
bb pg-regress jsonb expressions
```

The complete upstream corpus is a local discovery baseline rather than one
all-or-nothing CI gate. The pinned campaign accounts for PostgreSQL's full
`parallel_schedule`: application-facing files are assigned to compatibility
waves, and server-internal files are explicitly out of scope. Exact admitted
line slices are linked to focused tests and act as strict per-commit gates.

CI materializes the exact pinned PostgreSQL tag and validates the complete
campaign inventory on every commit. Use `bb pg-regress-setup` to create the
same ignored checkout locally without modifying an existing `../postgres`
tree.

A discovery run that produces differences exits successfully and retains its
full output under `.internal/pg-regress/`; a harness failure still fails. The
summary highlights frequent target errors and internal-looking failures so
unsupported surface does not hide class casts, unknown Datalog variables, or
lost connections.

Use `PG_REGRESS_STRICT=1` only for an admitted test that is expected to match
completely. Endpoint, PostgreSQL checkout, and binary overrides are documented
in `test/integration/postgres-regress/README.md`.

## Beta-exit coverage

The release-facing matrix, open blockers, manual release gates and explicit
non-goals live in [beta-exit.md](beta-exit.md) and
`test/integration/beta-exit.edn`. `bb beta-exit` validates that every named
evidence path and CI job exists and that every required job gates deployment.

## Golden-file catalog tests

The unit suite records the catalog results used by pgjdbc, Hibernate, and
other clients in `test/goldens/`. The probes in
`datahike.test.catalog-goldens-test` compare those exact row sets on every PR,
catching silent regressions in `pg_class`, `pg_attribute`, `pg_index`, and
`pg_type` projection.

To regenerate an intentionally changed probe, call `check-probe!` with
`:regenerate? true` from a REPL, inspect the diff, and commit the changed EDN.
Missing goldens are reported explicitly instead of silently weakening the
baseline.

## Toolchain versions pinned in CI

| Tool     | Version | Why that version                                 |
|----------|---------|--------------------------------------------------|
| Clojure  | 1.12.4  | Matches deps.edn — same as the project.          |
| Java     | 21      | pgjdbc Gradle build hard-codes 17/21 toolchain.  |
| Python   | 3.11    | asyncpg wants 3.9+; Debian 3.11 is stable.       |
| Node     | 20.11   | node-postgres 8.x tested against LTS.            |
| Maven    | 3.9 (image default) | Hibernate 6 builds clean.              |

## Running a single layer locally

```
bb test                  # unit only
bb sqllogictest          # SQL conformance
test/integration/pgjdbc/setup.sh                   # one-time
test/integration/pgjdbc/run-one.sh ResultSetTest   # point at :15432
test/integration/asyncpg/setup.sh                  # python venv + asyncpg C build
test/integration/asyncpg/run.sh                    # runs pytest against :15432
```
