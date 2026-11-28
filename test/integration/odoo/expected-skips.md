# Odoo TestORM baseline

The harness (this directory's `run.sh`) boots pgwire and drives Odoo
19's `--init=base --test-tags=:TestORM`. The archived baseline
recorded **all 11 TestORM cases passing**:

```
odoo.tests.stats:  base: 13 tests 7.08s 454 queries
odoo.tests.result: 0 failed, 0 error(s) of 11 tests when loading
                   database 'datahike'
```

The harness exits 0 when it sees this line. `PASS_FLOOR` (default 9)
is the floor below which the runner reports a failure. Record the actual
exit status and test summary when refreshing this evidence.

A fresh run on Odoo 19 commit `007e86391e14` with the transactional
index and nested membership fixes also passed all 11 tests and exited 0
with `--init=base --test-tags=:TestORM --skip-auto-install`.
The default run additionally initializes auto-installed modules, including web.

## Resolved blockers (history)

The prior `feature/postgres-wire-protocol` branch in `datahike`
reached 9–10/11. The remainder were closed on the `bugfix/cleanup`
branch by these translator fixes:

| Blocker | Where fixed |
|---|---|
| `DEFAULT (now() AT TIME ZONE 'UTC')` paren-strip preprocessor broke valid SQL | `sql.clj:preprocess-sql` — removed obsolete regex (JSqlParser 5.2 parses both forms natively) |
| `(now() AT TIME ZONE 'UTC')` rejected as `:unsupported` default | `sql/ddl.clj:column-default-spec` — recognised as `:fn now` (timezone wrapper is a no-op for `:db.type/instant`) |
| `INHERITS (parent_table)` and `(PRIMARY KEY(col))` only-body | `sql/rewrite.clj:primary-key-only-body-rule` — replaces the body with `(id serial)` so JSqlParser parses the surrounding CREATE TABLE |
| `<col> IN (SELECT …) IS NOT TRUE` raised `Encountered unexpected token: "IS"` | `sql/rewrite.clj:boolean-is-rule` — wraps the LHS of `IS [NOT] TRUE/FALSE/UNKNOWN` in parens so JSqlParser sees a boolean primary |
| `IN (subquery)` inside predicate-expr context (e.g. wrapped in IS NOT TRUE) raised 0A000 | `sql/expr.clj:translate-predicate-expr` — added `ParenthesedSelect` case that runs the subquery once and lifts to a value list |
| `LEFT JOIN … ON (a AND b)` fell through to the generic predicate path, breaking outer-join semantics | `sql/stmt.clj:translate-join` — `flatten-and` now also descends through size-1 `ParenthesedExpressionList` so each AND-conjunct routes through the per-EqualsTo branch |

## Outside the base gate

Account initialization and application workflows such as invoice posting,
module installation, and migrations have separate integration needs.
The base smoke does not qualify those paths. See the
[compatibility residuals](../../../doc/compatibility-residuals.md) for the
accounting findings.

## How to re-baseline

If a translator/runtime regression makes a previously-passing test
fail, edit this file with the new failure mode and lower the
assertion floor in `run.sh` (env var `PASS_FLOOR`) for triage:
`PASS_FLOOR=10 ./run.sh`. Re-tighten once fixed.

## Notes on the harness

- The script accepts the modern Odoo result-line format
  (`odoo.tests.result: <F> failed, <E> error(s) of <N> tests …`) and
  falls back to the legacy `Ran N tests in T s` form. If you change
  Odoo versions and the parser misses the format, both `TESTS_RAN` and
  `PASS_COUNT` come up empty in the `[run] …` summary.
- The script wipes `last-run.log` and the per-run `data/` filestore
  on every invocation. There is no incremental state — each run is
  a clean install of `--init=base`.
