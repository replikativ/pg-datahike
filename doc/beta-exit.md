# Beta-exit campaign

The target is pg-datahike 0.2.0. Stable means a dependable PostgreSQL
interface to Datahike for the surface we claim, not PostgreSQL feature parity.
The release rule is:

> No silent wrong answers or internal failures in the supported surface;
> unsupported behavior fails explicitly with a PostgreSQL SQLSTATE.

`test/integration/beta-exit.edn` is the campaign ledger. It names every release
gate, open blocker and deliberate non-goal. Validate and print it with:

```bash
bb beta-exit
```

## What the regression system proves

| Boundary | Current admission | What green means |
|---|---|---|
| Unit and focused regression tests | full suite, per commit | Known translator, execution, catalog, pgwire, temporal and fuzz findings remain fixed. |
| SQLLogic | full local corpus, per commit | The admitted application-facing SQL examples return their checked results. |
| Released secondary stack | focused Datahike/Scriptum/Proximum/Stratum vertical on JDK 25, per commit | Secondary creation, mutation, fallback, pagination, history and lifecycle contracts remain valid together. |
| pgjdbc | eight application-facing classes, 276 cases, per commit | 275 JDBC connection, statement, result, batch and metadata cases pass; one is skipped upstream. It is not complete JDBC conformance. |
| Hibernate | 14 application tests, per commit | Hibernate 6 DDL, CRUD, relationships, HQL and transaction flows work. |
| SQLAlchemy | 16 application tests, per commit | SQLAlchemy 2 with psycopg2 can perform the documented application flow. |
| asyncpg | 11 upstream modules, per commit | New per-test failures and module hangs fail CI; 67 known failures remain explicit. |
| node-postgres | 16 must-pass and 6 expected-failure files, per commit | The admitted JS client files stay green and known-gap files continue to run. |
| `pg_dump` | default COPY-format Pagila round-trip, per commit | Every compared table restores with the same row count and no COPY data failure. |
| PostgreSQL regression corpus | complete pinned 17.7 inventory plus admitted strict slices | Every scheduled upstream file is classified, and every strict slice points to a real focused regression test. |
| Odoo and Metabase | manual release gates | Their documented end-to-end application probes pass before a release candidate is promoted. |
| Datahike Server | version-pinned JAR and non-root container, per release | TLS/password authentication, abrupt client drops, graceful restart, durable catalog/data and post-restart writes work through the packaged server. |

This is behavioral coverage. We do not use source-line coverage as a release
percentage: a SQL translator can execute every branch and still return the
wrong rows, OIDs or SQLSTATE. Coverage grows by admitting observed behavior
from PostgreSQL, drivers and applications into a strict repeatable gate.

## Campaign status — 2026-09-04

- Unit: 1,630 tests / 7,003 assertions, all passing.
- SQLLogic: 61 assertion groups, all passing.
- Released secondary stack: 5 tests / 75 assertions, all passing on JDK 25.
- node-postgres: 16 admitted files passing, 6 known-gap files still xfail.
- pgjdbc: eight admitted classes, 275 passing and one upstream skip. This
  includes connection/read-only behavior, server-prepared statements, result
  handling, batch variants, JDBC 4.2 parameters and metadata properties.
- asyncpg: the pinned local and CI baseline is reconciled. Eleven upstream
  modules run per commit; 67 known failing test IDs remain explicit, and any
  new failure, unexpected pass or missing test fails the gate.
- PostgreSQL 17.7: all 222 scheduled files classified—57 campaign, 96 backlog
  and 69 deliberate non-goals. The campaign contains 100 admitted strict
  slices across 24 upstream files and 28 local gate files; 3 complete files
  are strict and 54 are measured discovery files.
- Datahike Server: the `0.8.1870` candidate embeds pg-datahike `0.1.189`.
  Its standalone JAR and non-root Podman image pass the restart, file
  persistence, TLS/password authentication and abrupt-client-drop soak.

## Where coverage is still weak

- pgjdbc breadth has been rerun and classified. Eight stable classes gate each
  commit; fixture-bound stored-function, identity-column, custom-type and
  database-wide-setting suites remain explicitly deferred.
- asyncpg's exact-set baseline is intentionally conservative: 67 known gaps
  still pass through the harness on every commit and must be retired as their
  underlying features become supported.
- node-postgres allowances are file-grained. One expected failure can hide a
  regression elsewhere in the same file. Convert the high-value files to
  per-test admission as their blockers are fixed.
- The PostgreSQL campaign has 57 application-facing files, but most remain in
  discovery mode. Its 100 strict slices provide real regression evidence; the
  raw upstream diff is not itself a pass/fail score.
- Odoo and Metabase are not yet per-commit jobs. They remain release gates
  until their runtime and setup costs are made reliable enough for CI.
- The Datahike Server JAR/container lifecycle gate is intentionally manual
  because it builds two large release artifacts and needs a container engine.
  Run both version-pinned scripts for every server release that updates the
  bundled adapter.

## Campaign waves

1. **Correctness:** three-valued NULLs, insert-then-delete in one transaction,
   embedded-NUL validation, alternating batch parameter types, temporary-table
   isolation and startup database validation.
2. **Runtime safety:** an enforced bound on result memory or result size.
3. **Compatibility admission:** reconcile asyncpg, widen and classify pgjdbc,
   and tighten node-postgres allowances.
4. **Release candidate:** run Odoo, Metabase and the version-pinned Datahike
   Server soak, then publish the evidence with the release candidate.

Fixes remove or close entries in `beta-exit.edn`; they must also promote the
relevant test from an expected-failure or manual observation into a gate.

## PostgreSQL source reproducibility

The upstream campaign is pinned to the tag in `campaign.edn`. Materialize that
checkout without changing an existing `../postgres` working tree:

```bash
bb pg-regress-setup
bb pg-regress-wave inventory
bb pg-regress-wave 1 discovery
```

The setup command writes under the ignored `.internal/` directory by default,
refuses to rewrite an existing checkout and verifies both the exact tag and
the relevant source cleanliness. CI runs the setup and inventory validation on
every commit. Full discovery waves remain local because their useful output is
triage, not a single compatibility percentage.
