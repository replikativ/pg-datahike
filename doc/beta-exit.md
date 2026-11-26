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
| Hibernate | 13 application tests, per commit | Hibernate 6 with the stock PostgreSQL dialect and JDBC simple-query mode covers schema, CRUD, relationships, HQL and transactions. |
| SQLAlchemy | 16 application tests, per commit | The custom Datahike dialect covers its documented SQLAlchemy and psycopg2 application flow. |
| asyncpg | 11 pinned upstream modules, per commit | The v0.30.0 manifest checks exactly 53 expected-failing owning IDs and rejects new failures, unexpected passes and missing cases. |
| node-postgres | 18 must-pass and four expected-failure files, per commit | The admitted JS client files pass and the known-gap files still run. |
| `pg_dump` | COPY-format Pagila row-count round-trip, per commit | Compared table counts agree and the COPY steps complete. The schema replay has its own residuals. |
| Differential fuzz | fixed-seed samples, per commit | Samples agree on rows or SQLSTATE except for the listed divergences. |
| PostgreSQL regression corpus | pinned 18.6 inventory and admitted strict slices | Scheduled files are classified, and each strict slice points to a real test. |
| Odoo and Metabase | manual application probes | Independent application runs have their own recorded fixtures, results and age. |
| Datahike Server | version-pinned JAR and non-root container, per release | TLS/password authentication, abrupt client drops, graceful restart, durable catalog/data and post-restart writes work through the packaged server. |

This is behavioral coverage. We do not use source-line coverage as a release
percentage: a SQL translator can execute every branch and still return the
wrong rows, OIDs or SQLSTATE. Coverage grows by admitting observed behavior
from PostgreSQL, drivers and applications into a strict repeatable gate.

## Campaign status — 2026-09-18

- Server identity: reports PostgreSQL 17.7, the pinned release.
- Unit: about 1,890 tests / 8,420 assertions, all passing.
- SQLLogic: 61 assertion groups, all passing.
- Differential fuzz: 842 SELECT, 113 prepared and 257 DML distinct samples at
  the CI seed; one listed divergence (`to_char` over dates).
- pgjdbc: eight admitted classes, 275 passing and one upstream skip.
- asyncpg: eleven modules per commit; 62 known failing test IDs remain
  explicit, and any new failure, unexpected pass or missing test fails the gate.
- node-postgres: 16 admitted files passing, 6 known-gap files still xfail.
- `pg_dump`: Pagila round-trip compares 21 tables, partitions included, with
  no mismatch.
- PostgreSQL 17.7: all 222 scheduled files classified: 80 campaign (3
  strict, 77 discovery), 73 backlog, 69 deliberate non-goals. 100 strict
  slices across 24 upstream files. A full discovery run matches 5 of 152
  application-facing files exactly; 19 still show internal failures.

### Index and constraint behavior

Catalog migration and initial constraint validation commit atomically before
a SQL handler becomes available. A failed validation leaves the database
unchanged. Independent transaction predicates can enforce constraints without
replacing one another.

Index backfill is synchronous and may require substantial memory for large
attributes. Transaction-local permission to backfill does not change the
database's persistent configuration. Uniqueness upgrades validate existing
current values, including attributes that already have an index.

Native scalar NaN writes and exact per-row sequence evaluation order remain
compatibility limitations. These features do not yet satisfy the beta-exit
criteria.

## Where coverage is still weak

The current [compatibility residuals](compatibility-residuals.md) track the
upstream PostgreSQL and client findings, the manual application evidence,
and the gaps in the test setup. The dated campaign snapshot above is
historical. Run the version-pinned Datahike Server artifact tests for a
server release that updates the bundled adapter.

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
