# Integration testing model

pg-datahike is tested at three layers, with CI wired into two
different cadences.

## Layer 1 — unit (per-commit)

```
bb test             234 tests / 952 assertions — ~30 s
bb sqllogictest     59 files — ~60 s
```

Run on every PR + commit. Covered by the `unittest` and `sqllogictest`
jobs in the main workflow.

Exercises the translator, handler dispatch, classifier, rewriter,
shape matcher, and SQL-conformance surface. Does **not** involve a
real wire connection.

## Layer 2 — wire + ORM conformance (per-commit)

Three jobs run against a live pgwire server on :15432:

```
pgjdbc-conformance       80 ResultSetTest cases — ~6 min warm daemon
hibernate-app-conformance 13 end-to-end tests — ~2 min
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

## Layer 3 — low-level wire regression targets (nightly, non-blocking)

| Harness          | Base image        | Client binding    | Status against HEAD (0.1)                    |
|------------------|-------------------|-------------------|----------------------------------------------|
| asyncpg          | cimg/python:3.11  | C-extension async | ~32 / ~67 — prepared-statement + cursor gaps |
| node-postgres    | cimg/node:20.11   | pure-JS `pg`      | ~4 / many — `COLLATE` parse + protocol gaps  |

Runs on a schedule (`nightly-integration` workflow, 04:00 UTC daily,
main only). The harness and setup scripts are verified to install +
invoke cleanly (see `test/integration/<name>/README.md` +
`expected-skips.md`); what's not ready is the translator's coverage
of what these clients probe. Both stay in nightly so a future fix
will immediately light them up, without blocking per-commit PRs on
known gaps.

For 0.1 release, we don't block on them being green. Hibernate +
SQLAlchemy (full ORM round-trips via pgjdbc and psycopg2) moved
up to Layer 2 once they hit 100% on their curated suites.

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

## PostgreSQL's upstream regression suite

`bb pg-regress` runs PostgreSQL's own `pg_regress` driver, SQL, and expected
output against an existing pg-datahike server. It uses `../postgres` and the
installed PostgreSQL 17 tools by default:

```
bb pg-regress jsonb
bb pg-regress jsonb expressions
```

This is initially a baseline rather than a CI gate. A completed upstream test
that produces differences exits successfully and retains its full output under
`.internal/pg-regress/`; a harness failure still fails. The summary highlights
frequent target errors and internal-looking failures so unsupported surface
does not hide class casts, unknown Datalog variables, or lost connections.

Use `PG_REGRESS_STRICT=1` only for an admitted test that is expected to match
completely. Endpoint, PostgreSQL checkout, and binary overrides are documented
in `test/integration/postgres-regress/README.md`.

## Golden-file catalog tests (planned)

pgjdbc / Hibernate / Rails / Django all call a small fixed set of
`DatabaseMetaData` methods at connection time. Each method issues a
specific SQL statement against `pg_catalog` tables. Recording the
expected row shapes once and diffing on every PR catches silent
regressions in `pg_class` / `pg_attribute` / `pg_index` projection —
a class of bug that unit tests miss because no unit test knows which
exact 47 columns Hibernate wants.

Sketch:

```
  test/goldens/pgjdbc-getTables.edn       # per-driver, per-call
  test/goldens/pgjdbc-getColumns-person.edn
  test/goldens/hibernate-boot-probe.edn
  ...
```

Runner reads the golden, executes the SQL, compares. A diff prints the
row-set diff + prompts the dev to intentionally regenerate the golden
(`bb goldens :update`). Planned for the same session that adds the
cross-engine runner — they share harness code.

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
