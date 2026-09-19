# Integration testing model

pg-datahike is tested at five complementary boundaries. The jobs in layers
1–3 and the differential fuzzer run for every pull request; suites with known
upstream gaps use explicit manifests so only a new regression fails the build.
Layer 4 is a local discovery and admission workflow. The [coverage
map](#coverage-map) below shows which of them covers which behaviour.

## Layer 1 — unit (per-commit)

```
bb test
bb sqllogictest
```

A current full run reports about 1,890 tests / 8,420 assertions and the
SQLLogic runner reports 61 assertions. Treat the runner output, rather than these
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
pgjdbc-conformance       276 cases in 8 classes — ~6 min warm daemon
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

## Differential fuzzing (per-commit)

`datahike.fuzz.differential` generates SQL, runs every sample on a real
PostgreSQL 17.7 and on pg-datahike, and diffs the answers. Hand-written
tests check what their author suspected; the fuzzer checks what nobody
thought of. Its first run found 58 disagreements, including two regressions
the hand-written tests of the same PRs had missed (#75).

```
REFERENCE_URL=... TARGET_URL=... bb fuzz [select|prepared|dml|all] [n] [seed]
```

| Surface | Generated | Compared |
|---|---|---|
| `select` | ~50 classes: comparisons, 3-valued logic, arithmetic and numeric edges, CASE, casts, string/date/timestamp functions, arrays, jsonb, aggregates, GROUP BY/HAVING, DISTINCT (ON), joins, self-joins, set operations, window functions (frames, ranking, FILTER, top-N), CTEs, recursion, correlated subqueries, LATERAL | rows, or SQLSTATE when both fail |
| `prepared` | parameterised predicates, projections and aggregates with NULL and edge parameters over the extended protocol | rows, or SQLSTATE |
| `dml` | INSERT (VALUES and SELECT), UPDATE, DELETE; both sides re-seeded per sample | row count or SQLSTATE, plus the resulting table |

The `differential-fuzz` job runs a fixed seed against a PostgreSQL 17.7
sidecar and gates deployment. Known divergences are listed, each with a
reason, in `test/integration/fuzz/expected-divergences.edn`. An unlisted
disagreement fails, and so does a listed one that now agrees. A new seed is
how to look for new bugs locally; see `test/integration/fuzz/README.md`.

Comparing SQLSTATEs rather than "both failed" matters: when it was
introduced it found casts reporting 22P02 instead of 42846, and INSERT
VALUES storing the SQL text of expressions it could not evaluate.

Function breadth is measured separately and does not gate: `bb fncov` calls
every buildable `pg_catalog` overload on both servers and lists the functions
that answer differently (`:wrong`) or not at all (`:missing`).

Not generated yet: DDL and ALTER sequences, catalog views after DDL,
transactions and savepoints, `ON CONFLICT` / `RETURNING`, COPY, and
anything beyond the fixed two-table schema. These are the next surfaces.

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

Status (pinned REL_17_7, 2026-09-18): 222 scheduled files, of which 80 are in
the campaign (3 strict, 77 discovery), 73 in the backlog and 69 deliberately
out of scope. 100 strict slices (about 1,300 lines of upstream SQL) are
admitted as focused tests. A full discovery run over the 152
application-facing files matches PostgreSQL's expected output exactly for 5
(`boolean`, `delete`, `md5`, `portals_p2`, `select_having`); 19 still show
internal failures, the first thing to eliminate under the beta rule.

Use `PG_REGRESS_STRICT=1` only for an admitted test that is expected to match
completely. Endpoint, PostgreSQL checkout, and binary overrides are documented
in `test/integration/postgres-regress/README.md`.

## Coverage map

Which harness exercises which behaviour. "Gate" means a per-commit failure;
"discovery" means measured but not gating.

| Behaviour | Unit / focused | SQLLogic | Client suites | Differential fuzz | PostgreSQL regression |
|---|---|---|---|---|---|
| Expressions, NULL logic, casts, numeric edges | gate | gate | incidental | gate (`select`) | discovery (`boolean` matches exactly) |
| Built-in functions and operators | gate | partial | incidental | gate for the generated set; breadth measured by `bb fncov` (not gating) | discovery, many type files in backlog |
| Aggregates, GROUP BY, HAVING, DISTINCT | gate | gate | incidental | gate | discovery (`select_having` matches) |
| Joins, subqueries, correlation, LATERAL, CTEs | gate | partial | ORM queries | gate | discovery (`join`, `subselect`, `with`) |
| Window functions | gate | — | — | gate | discovery (`window`) |
| INSERT / UPDATE / DELETE | gate | gate | ORM CRUD | gate (`dml`) | discovery (`delete` matches) |
| `ON CONFLICT`, `RETURNING` | gate | — | ORMs | — | discovery (`insert_conflict`) |
| Extended protocol, parameters, codecs | embedded JDBC tests | — | gate: pgjdbc, asyncpg, node-postgres | gate (`prepared`) | — |
| DDL and catalog / introspection | gate + catalog goldens | — | gate: ORM boot, SQLAlchemy/asyncpg introspection, pgjdbc metadata; psql by hand | — | backlog (`create_table`, `alter_table`) |
| Transactions and savepoints | gate | — | gate (clients) | — | discovery (`transactions`) |
| COPY and dump restore | gate | — | gate: pg_dump round-trip | — | discovery (`copy`, `copy2`) |
| SQLSTATEs of failures | gate | — | asserted by clients | gate (every sample) | discovery |

The regression suite is the widest source of cases but the weakest gate: CI
only validates its inventory, and the strict slices are admitted as focused
unit tests. The fuzzer is the widest gate for query semantics. DDL,
catalogs, transactions and COPY rely on unit tests and client suites, and
have no generated coverage yet.

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
