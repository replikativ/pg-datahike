# Differential fuzzing

`datahike.fuzz.differential` generates SQL, runs every sample on a real
PostgreSQL 17.7 (the oracle) and on pg-datahike, and diffs the answers. It
finds what nobody thought to test: its first run turned up 58
disagreements, two of them regressions the hand-written tests of the same
PRs had missed (#75), and it drove the correctness work through #96.

| surface     | what it generates | protocol |
|-------------|-------------------|----------|
| `select`    | ~50 query classes over a NULL-heavy table: projections, predicates, joins, aggregates, windows, CASE, casts, arrays, jsonb, CTEs, correlated subqueries, LATERAL, recursion | simple |
| `prepared`  | parameterised shapes, NULL and edge parameters | extended (Parse/Bind/Describe/Execute) |
| `dml`       | INSERT / UPDATE / DELETE; both sides re-seeded, then row count and resulting table compared | simple |

**Agreement** means identical rows (as text, in order; every generated query
orders fully), or both sides failing with the **same SQLSTATE**. Error
wording is not compared. An error where PostgreSQL answers, or a different
SQLSTATE, is a disagreement. This matters most for our XX000 where
PostgreSQL raises something specific, which the beta rule forbids.

Known disagreements live in `expected-divergences.edn`, each with a reason.
An unlisted disagreement fails the run, and so does a listed one that the
run draws and that now agrees (prune it).

## Running

    # oracle: any PostgreSQL 17.7; target: a running pg-datahike
    REFERENCE_URL='jdbc:postgresql://127.0.0.1:5432/postgres?user=pgtest&password=pgtest' \
    TARGET_URL='jdbc:postgresql://127.0.0.1:15432/datahike?user=datahike&password=datahike' \
      bb fuzz [select|prepared|dml|all] [n] [seed]

Defaults: all surfaces, 1500/600/300 samples, seed 20260918. The generator
is seeded, so a seed reproduces its queries exactly. CI runs the fixed seed
(gating). A new seed is how you look for new bugs, and anything it finds
belongs in a test or in the manifest.

From a REPL in the server JVM:

    (require '[datahike.fuzz.differential :as fz])
    (fz/report (fz/run-surface :select 1500 42))

When a disagreement is real, pin it as a regular test (see
`test/datahike/test/pg_fuzz_findings_test.clj`) with the oracle's answer
as the expectation, then fix it.
