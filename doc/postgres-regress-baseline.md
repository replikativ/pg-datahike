# PostgreSQL regression baseline

The regression campaign is pinned to PostgreSQL 17.7 (`REL_17_7`).
The schedule inventory records which files have been classified; the output
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
| Files measured | 154, including the unscheduled `numeric_big` |
| Files with 100% expected-line coverage | 16 |
| Mean per-file agreement | 64.4% |
| Approximate line-weighted agreement | 55.5% |

The agreement runner also discovers SQL files outside the parallel schedule.
Its denominator therefore differs from the schedule inventory. The pinned
SQL directory contains 224 files; after excluding the 69 files in
`scope.edn`, 155 are candidates, of which 154 have recorded scores.
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
