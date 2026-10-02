# Review backlog

Findings from the October 2026 review sweep that are **not** fixed, each
with what it costs and why it was deferred. Everything here was
reproduced against the pinned PostgreSQL 17.7 oracle; nothing is a
guess. Items are removed as they land.

The fixed findings are in the git history (PRs #270–#275); this file is
only what remains.

## Needs an architectural change

### COPY FROM does not fire row triggers
`CopyFrom` calls `ExecBRInsertTriggers` per row; we call none, so a
BEFORE trigger that rewrites or suppresses a row is silently bypassed
by the one bulk path where that matters most.

Firing them is not the hard part — `server.clj`'s COPY row loop can
capture `*statement-handler*` at `start-copy-in!` (the data arrives in
later protocol messages, so the dynamic binding is gone by then) and the
body then runs, with its rewrite of `NEW` taking effect. What does *not*
work is any statement the body itself issues: an audit
`INSERT INTO log` inside the trigger vanishes, because COPY batches onto
its own basis and the nested write is not part of it. **Silently losing
a trigger's writes is worse than not firing**, so the half-fix was
reverted. This needs COPY's batch/transaction model to carry nested
statement effects.

### ON CONFLICT DO UPDATE fires no UPDATE-side triggers
`INSERT … ON CONFLICT DO UPDATE` now fires BEFORE INSERT ROW for the
proposed row, which is what PostgreSQL does first and is verified
identical. It does **not** fire BEFORE/AFTER UPDATE ROW for the branch
that updates:

```
-- BEFORE INSERT and BEFORE UPDATE both do NEW.b := NEW.b + 500
INSERT INTO c1 VALUES (1,7) ON CONFLICT (a) DO UPDATE SET b = excluded.b;
ours   1|507      PostgreSQL 1|1007
```

The update branch is inside `reduce-on-conflict` in `sql/stmt.clj` —
the translator — and the trigger machinery is in `server.clj`. Wiring
one to the other is a layering decision, not a patch.

### Generated columns and BEFORE ROW triggers
STORED generated columns are computed in `materialize-insert-candidate`,
which runs before `fire-row-triggers`. `nodeModifyTable.c`'s order is
BEFORE trigger → constraints → `ExecComputeStoredGenerated` → insert, so:

```
CREATE TABLE g (a int, b int GENERATED ALWAYS AS (a * 2) STORED);
-- BEFORE INSERT trigger does NEW.a := NEW.a + 100
INSERT INTO g(a) VALUES (1);   ours a=101 b=2    PostgreSQL a=101 b=202
```

and `NEW.<generated>` is visible to a BEFORE trigger where PostgreSQL
guarantees NULL. `ON CONFLICT DO UPDATE` never recomputes a generated
column either, and accepts a literal assignment to one.

### The datetime tokeniser
A 855-pair sweep (171 literals × 5 targets) against the oracle found
**262 divergences**. They are not independent: `trailing-zone-re`,
`peel-named-zone` and `time-input` are three regexes standing in for
`ParseDateTime`'s tokeniser, and every divergence is a token they cannot
spell — a one-digit offset (`+5`), a seconds offset (`+05:30:30`),
`GMT+8`, `PST8PDT`, a weekday, an era behind a zone, a month name in
front of a time (`Mon Feb 10 17:32:01 1997`, which is PostgreSQL's own
`Postgres` output style and how `timestamp.sql` seeds its table).

Step 1.3's premise — read the literal once — is right, but it is still
being read by pattern-match rather than tokenised, and `datetktbl` /
`deltatktbl` are not in play at all. Related and independent: the
infinity sentinel leaks through `date`/`time` casts and `extract`
(`'infinity'::date + 1` → year 292278994), binary parameter decoding has
no infinity case at all (`PgParamCodec.java:1189` wraps), `timestamptz`
drops `+00` on the all-constant path, and temporal typmod
(`timestamp(3)`) is parsed and discarded.

### pg_dump wedges the server
A single-table `pg_dump` takes ~7 minutes, reaches 8.9 GB RSS and leaves
the server unusable. Independent of every PR in the review.
`copy-to-lines` materialising the whole result three times
(`.rows`, the encoded `String` vector, then `into-array`) is a plausible
contributor — PostgreSQL streams row by row — but this was not
reproduced and should be measured before it is believed.

## Wrong answers, contained, not yet done

- **`&&`, `@>`, `<@` on geometric values return a bare `f`** —
  `expr.clj`'s array/jsonb fall-through. `'(0,0),(1,1)'::box &&
  '(0,0),(2,2)'::box` is `t` in PostgreSQL.
- **`ON DELETE CASCADE` is not applied** — `DELETE FROM parent`
  succeeds and leaves the child row behind, silently creating orphans.
- **`COMMIT` inside a `DO` block commits the caller's transaction**;
  the client's `ROLLBACK` then does nothing.
- **`DO` silently drops an EXCEPTION handler** that `CREATE FUNCTION`
  honestly refuses.
- **CSV `COPY … FROM STDIN` does not recognise `\.`** — documented as
  intentional in `copy/csv_format.clj`, but `copyfromparse.c` accepts it
  as the first character of a line in CSV mode too. Every CSV stdin
  block in `copy.sql`/`copy2.sql` either gains a junk row or fails.
- **`COPY tbl TO` emits generated columns**; `CopyGetAttnums` excludes
  them from the implicit list.
- **Collection aggregates do not preserve scan order** —
  `array_agg(v)` over 1,2,2,5 answers `{5,1,2,2}`. PostgreSQL does not
  guarantee an order without ORDER BY, but it returns scan order and the
  regress suite compares text.
- **`string_agg` over bytea** emits `[B@2e349857`.
- **The generated CHECK name is wrong for the single-column ALTER
  case** — `ChooseConstraintName` uses `<table>_<col>_check` when the
  expression references exactly one column; the ALTER path passes nil
  for the column, so CREATE TABLE and ALTER now disagree.

## Missing validation (we accept what PostgreSQL rejects)

- **COPY implements none of `copy.c`'s ~23 cross-option checks**, and
  an invalid option *still enters COPY-IN mode*, swallowing the
  statements that follow. ~23 cases catalogued in the review.
- **Every generated-column restriction is unenforced**, including one
  that makes the same schema give different answers depending on column
  order.
- **An FK is never checked for a unique constraint on the referenced
  columns** (PostgreSQL: 42830) unless the parent has no PK at all.
- **`NOT VALID` / `VALIDATE CONSTRAINT`** — the first is a parse error,
  the second answers `ALTER TABLE` for a constraint that does not exist.
- **Duplicate PREPARE/DECLARE names**, and `EXECUTE` with a missing
  argument substitutes a literal NULL.
- **Row-level transition tables** are accepted at CREATE TRIGGER and
  then fail at run time with `relation "nr" does not exist`. Either wire
  `:transitions` through `fire-after-row-triggers!` or reject the DDL.

## Catalog honesty

- **`pg_attrdef`/`pg_get_expr` are empty for a generated column**, so
  `pg_dump` loses the generation clause entirely.
- **`information_schema.columns.is_generated`** is hardcoded `"NEVER"`.
- **`pg_get_triggerdef`** returns NULL for every trigger.
- **`pg_class.relhastriggers`** is NULL while `pg_tables.hastriggers`
  is correct — `\d` and many tools read the former.
- **`tgattr`** is empty for `UPDATE OF b`; `tgoldtable`/`tgnewtable`
  are NULL for a trigger that declares transition tables.
- **`pg_proc` holds no user functions.**
- **The geometric catalog advertises ~25 functions and ~30 operators
  that do not resolve** — `pg_proc` has `box_eq`, `diagonal`,
  `box_intersect` and the rest, loaded from the generated data. A
  client that resolves through the catalog acts on a promise we do not
  keep.
- **`TG_TABLE_SCHEMA` and `TG_RELID`** are not bound (42703).
- **`DROP FUNCTION f()` succeeds while a trigger depends on it**
  (PostgreSQL: 2BP01 naming the trigger); the next INSERT then dies
  with 42883.
- Error-text detail: runtime FK violations carry no table or constraint
  name and no DETAIL; CHECK violations have no
  `DETAIL: Failing row contains (…)`; plpgsql errors carry no
  `CONTEXT:` line; COPY FROM errors carry no `CONTEXT: COPY t, line N`,
  which pg_regress compares.

## Performance

- **COPY FROM's per-row prologue.** The 12.5s/10k figure is ~85%
  datahike's datom writes, but the COPY-specific 56–125 µs/row is
  `enrich-schema-with-pg-array-meta` over the full schema, *two*
  catalog-basis comparisons and a rebuilt `column-defaults` — once per
  row, because the chunk is sliced at every newline. Every row is also
  validated **twice** (`copy-flush-batch!` then `publish-copy!`), and
  `coerce-field` redoes three schema lookups plus `pg-name->oid` per
  field.
- **Trigger dispatch is O(rows)**: `trigger-ast` re-parses the function
  body per row per trigger, `when-condition-holds?` re-parses the WHEN
  text per row, and `before-row-insert-triggers?` runs a Datalog query
  plus `read-string` inside the per-candidate loop — on every table,
  including those with no triggers. Measured 591 ms vs 293 ms for a
  1000-row `INSERT … SELECT` with one trivial trigger.
- **Geometric input** was ~2.5 µs/point and ~16 µs/polygon before the
  scanner rewrite. The rewrite was not measured cleanly (load average
  24 on the box at the time) and should be, on an idle machine, before
  anything is claimed either way.
