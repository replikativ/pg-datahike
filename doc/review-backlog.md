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

### plpgsql EXCEPTION handlers need subtransactions
`DO` and `CREATE FUNCTION` both refuse a body with an EXCEPTION handler
(0A000) since #278 made them consistent. The parser already produces
`:exception-handlers` with their conditions and bodies, a condition-name
to SQLSTATE table already exists for `RAISE`, and errors already travel
as ExceptionInfo with a `:sqlstate` — so catching and dispatching is a
small change.

**It would be a wrong answer without subtransaction rollback.** Entering
a handler rolls back what the block did, and `mvcc.sql` tests exactly
that: the block inserts 100 rows, raises, and the handler swallows it,
after which the file asserts those rows do **not** exist. A handler that
runs without rolling back leaves them, which is worse than the refusal —
it is the silent wrong answer this campaign exists to remove.

So this is gated on a savepoint/subtransaction mechanism, not on the
handler itself. It costs one exact-match file (`mvcc`, 100.0 → 90.5 when
the refusal replaced silently dropping the handler) and is the largest
single agreement item left.

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

- **`NOT VALID` is still a parse error** (`ALTER TABLE … ADD CHECK (…)
  NOT VALID`). `VALIDATE CONSTRAINT` is implemented, so only the
  deferred-validation half is missing.
- **`EXECUTE` with a missing argument** substitutes a literal NULL.
- **An explicitly named COMPOSITE constraint reports the default name.**
  `CONSTRAINT pk1 PRIMARY KEY (x,y)` appears in `pg_constraint` as
  `t_pkey`: the lowering computes the name and does not persist it on
  the derived tuple attribute, so the catalog re-derives PostgreSQL's
  default.

- **3 of PostgreSQL's 21 `&&`/`@>`/`<@` geometric pairs are
  unimplemented** — polygon-to-polygon overlap and containment, which
  need `lseg_inside_poly` and segment intersection. They refuse with
  0A000 rather than answering `f`; the other 18 are implemented.
- **Collection aggregates do not preserve scan order** —
  `array_agg(v)` over 1,2,2,5 answers `{5,1,2,2}`. PostgreSQL does not
  guarantee an order without ORDER BY, but it returns scan order and the
  regress suite compares text.

## Missing validation (we accept what PostgreSQL rejects)

- **`ON_ERROR` and `LOG_VERBOSITY` are accepted and ignored** — the
  option now passes the cross-option checks and still does nothing, so
  `ON_ERROR ignore` stops at the first bad row instead of skipping it.
  `HEADER`/`HEADER MATCH` in TEXT format are likewise accepted and
  unimplemented (PostgreSQL allows HEADER in text mode; only BINARY
  rejects it).
- **Every generated-column restriction is unenforced**, including one
  that makes the same schema give different answers depending on column
  order.

## A design question, not a defect

### `information_schema.columns` lists `db_id`; `pg_attribute` does not
The synthetic `db_id` column — Datahike's entity id, and `SELECT db_id
FROM t` really works — is prepended to `information_schema.columns` and
absent from `pg_attribute`. So the two catalogs disagree about a table's
columns, and every `ordinal_position` is one higher than PostgreSQL's:
`id` reports 2 where PostgreSQL says 1, for every table.

Hiding it from `information_schema` makes both catalogs agree and
matches PostgreSQL's ordinals, and two tests fail — including one
asserting `is_identity = YES` on `db_id`, which was written on purpose.
Listing it in `pg_attribute` instead makes them agree the other way and
keeps a column PostgreSQL has no equivalent for. Either is defensible
and it changes what tools see, so it wants a decision rather than a
drive-by fix.

## Catalog honesty

- **`pg_proc` holds no user functions.**
- **The geometric catalog advertises ~25 FUNCTIONS (`area`, `center`,
  `diameter`, `npoints`, …) and the `<->` distance operator, none of
  which resolve** — `pg_proc` has `box_eq`, `diagonal`,
  `box_intersect` and the rest, loaded from the generated data. A
  client that resolves through the catalog acts on a promise we do not
  keep.
- Error-text detail, what remains: plpgsql errors carry no `CONTEXT:`
  line, and COPY FROM errors carry no `CONTEXT: COPY t, line N` -- both
  compared by pg_regress. The unique / FK / NOT NULL / CHECK messages
  and their DETAILs now match.
- **A composite unique violation's DETAIL is untested against the
  oracle.** The formatter builds `Key (x, y)=(1, 1) already exists.`
  from `:columns`/`:value`, and the single-column form is verified; the
  composite path throws from a different site and was not diffed.

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
