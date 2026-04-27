# Odoo TestORM — current state

The harness (this directory's `run.sh`) boots pgwire and drives Odoo
19's `--init=base --test-tags=:TestORM`. As of the current commit it
fails during the bootstrap-DDL phase, not in TestORM itself — Odoo's
`base_data.sql` exercises CREATE TABLE shapes the translator doesn't
yet cover. Each shape is a discrete fix; the harness identifies them
in execution order so a developer can chip away.

## Resolved by the harness landing

| Issue | Where fixed |
|---|---|
| `DEFAULT (now() AT TIME ZONE 'UTC')` paren-strip preprocessor broke valid SQL | `sql.clj:preprocess-sql` — removed obsolete regex (JSqlParser 5.2 parses both forms natively) |
| `(now() AT TIME ZONE 'UTC')` rejected as `:unsupported` default | `ddl.clj:column-default-spec` — recognised as `:fn now` (timezone wrapper is a no-op for `:db.type/instant`) |

## Next blockers (in order)

These are the shapes the harness will hit next; each is a discrete
translator gap rather than a fundamental design issue:

1. **`INHERITS (parent_table)`** — `CREATE TABLE ir_act_window …
   INHERITS (ir_actions)` is PG's table-level inheritance. We need to
   either implement it (clone the parent's columns onto the child) or
   reject it cleanly so Odoo's fallback path runs.
2. **`jsonb` columns and `name jsonb NOT NULL` defaults** — Odoo
   stores translatable strings as JSONB. We have JSONB get-text /
   path operators but the column declaration + value coercion path
   may need work.
3. **Multi-statement bootstrap script via Extended Query** —
   psycopg2's `cursor.execute(big_string)` may go through Parse rather
   than Simple Query. Our handleParse is single-statement; a
   bootstrap script with 12 CREATE TABLE + 12 INSERT requires
   per-statement dispatch in the Parse path too.

## Historical baseline (pre-extraction)

The prior `feature/postgres-wire-protocol` branch in `datahike`
reached **9–10 / 11 passing** in TestORM via a manual workflow
(commit messages in that branch reference `Odoo TestORM 8/11 → 9/11`
etc.). Once the bootstrap blockers above are resolved this harness
should reproduce that baseline. Two TestORM cases known to fail then
for non-translator reasons:

- `test_try_lock_for_update` — Odoo's test framework opens nested
  pgwire sessions; our row-lock registry treats each independently.
- `test_access_filtered_records` — `ir.rule.domain_force` server-side
  Python eval hits a translator path we don't yet cover.

## Excluded permanently

Anything outside `--test-tags=:TestORM`. The full Odoo test suite
(`account.tests.*`, etc.) touches Postgres-only features (LISTEN /
NOTIFY, replication, COPY, large objects, server-side procedures)
that pg-datahike is explicitly out of scope for.

## Excluded permanently (feature gaps we don't implement)

Anything outside `TestORM`. The full Odoo test suite (`base.tests.*`,
`account.tests.*`, etc.) would touch Postgres-only features (LISTEN /
NOTIFY, replication, COPY, large objects, server-side procedures)
that pg-datahike is explicitly out of scope for.

## How to re-baseline

If a translator/runtime improvement makes a previously-failing test
pass, edit this file: move the test from "known failures" to "passing"
and bump the assertion floor in `run.sh` accordingly. The harness's
`PASS_FLOOR` env variable lets you experiment without committing
first: `PASS_FLOOR=10 ./run.sh`.
