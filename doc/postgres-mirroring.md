# Mirroring a live PostgreSQL, with our semantics

Status: **proposal, nothing implemented.** This file holds the decisions
and the constraints. The evidence behind them -- XTDB 2.2 line
references, a captured `postgres_fdw` session against a scratch server,
and a per-step verdict on what we answer today -- is in the research
note `.internal/xtdb-external-sources.md` (local, not committed, and its
file:line references drift like the other `.internal/` reports).

## The goal

A pg-datahike database configured as a mirror of
`{host, db, publication, slot}`: it snapshots a publication, follows
pgoutput, and applies each upstream commit as one `d/transact`. Clients
then query it with our PostgreSQL semantics **plus** `FOR SYSTEM_TIME AS
OF` -- time travel over a database that never had it. XTDB 2.2 shipped
the same shape and has shown the demand.

## The three shapes, and which ones we want

| | What | Verdict |
|---|---|---|
| **Option 1** | upstream PostgreSQL → pg-datahike live mirror (CDC in) | **The strategic feature.** ~4-6 wk MVP |
| **Option 2** | a running PostgreSQL querying us through `postgres_fdw` / `dblink` | **Cheapest win, do first.** ~1.5-2.5 wk, and most of it is catalog work this plan already owns |
| **Option 3** | pg-datahike as a logical-replication publisher (CDC out) | **Defer.** Needs a durable commit cursor and consumer-pinned GC roots in datahike, after the 1.0 id layout |

Option 2 first, then Option 1. Option 2 is independently useful, and it
builds the PG-17-in-tests infrastructure and catalog fidelity that
Option 1 also needs.

## Where we beat XTDB, and what not to copy

**Copy:** the resume token written in the same transaction as the rows;
resume from what is *indexed* but confirm only what is *durable*; one
sole confirmer; pgjdbc's automatic flush turned **off** (it confirms the
slot from keepalives behind your back -- the single most important
non-obvious detail); skip re-delivered LSNs.

**Do better, because a datom per attribute lets us:**

- **Unchanged TOAST.** pgoutput sends `'u'` for an unchanged out-of-line
  value. XTDB writes whole documents, so it must halt and demand
  `REPLICA IDENTITY FULL`. We emit *no datom for that attribute* and the
  previous value stays current. **We do not need REPLICA IDENTITY FULL
  for TOASTable columns.**
- **TRUNCATE.** XTDB halts on the `T` message. We retract the marked
  rows in the same transaction, and with `:keep-history? true` an `AS OF`
  before the truncate still sees the data.
- **Real keys.** No synthetic `_id` column: we have the PK via
  `:db.unique/identity`, and a composite PK via `:t/pg$pk_tuple`.

**Do not copy:** halting on TRUNCATE or on an unchanged TOAST value;
tables added to the publication later that are never snapshotted; the
"inoperable, re-ATTACH everything" answer to an interrupted snapshot
(a staging branch gives us an atomic area instead).

## Constraints, to settle before any code

These exist because a mirror is a second writer, and this repository's
whole consolidation effort is about not having two implementations of
one concept.

1. **One write path.** Replica mode -- skipping FK/CHECK/UNIQUE during
   apply, PostgreSQL's `session_replication_role = replica` -- is a
   **flag on the existing path**, never a parallel one, and it must be
   visible in status so nobody later has to guess which rows were
   checked. Upstream already enforced those constraints, possibly
   `DEFERRABLE INITIALLY DEFERRED`, so a per-row immediate check in
   commit order can fail spuriously. That is the reason for the flag; it
   is not a reason for a second path.
2. **Everything goes through `transact-recorded!`.** Its docstring says
   every server-side transact must, because of the conflict ring. The
   mirror needs tx-meta, so `transact-recorded!` grows an optional
   tx-meta argument -- it does not grow a bypass.
3. **Confirm only what is durable, and confirm from one place.**
4. **The LSN lives in the data,** as tx-meta plus a single cursor entity
   upserted in the same transaction. Never derive it from tx ids.
5. **No code may assume dense tx ids.** See the `ring-write-eas` note
   below.
6. **Refuse a mirror on a volatile store** (`:memory`, a non-fsyncing
   backend) unless explicitly allowed: "durable" is the whole argument.

## Seams to land early

Independently useful, and cheap. Doing them now means nothing built in
the meantime has to be reworked, and the mirror never has a standing
reason to go around the one required write path.

- **Optional tx-meta on `transact-recorded!`** (constraint 2).
- **A database-level read-only flag.** Every client DML/DDL answers
  `25006`, `default_transaction_read_only` reports `on`, and only the
  replicator's internal handle writes. Useful on its own for a read-only
  branch or replica, and it is what makes the sole-writer argument
  behind the cursor sound.
- **`ring-write-eas` density.** It assumes `max-tx` increments by
  exactly one per transaction. Under a Datomic-style shared `t` counter
  (`tx = (3<<42)|t`) the difference over-counts, every window becomes
  `::gap`, and every COMMIT falls back to a full-database conflict scan
  -- about 1 s at 3M datoms. 1-2 days, no prerequisites, and required
  before the 1.0 ids regardless of whether we ever build a mirror.

## Prerequisites this plan already owns

- **Catalog fidelity** (consolidation-plan Phase 4 and the `sysviews`
  cluster). A mirror synthesizes `CREATE TABLE` / `ALTER TABLE` SQL and
  runs it through our own DDL path -- deliberately, so the translator,
  catalog and `\d` all keep working -- and it reads upstream
  `pg_attribute` / `pg_constraint`. It inherits whatever the catalog
  gets wrong. Today `pg_class` serves 6 of PostgreSQL 17's 33 columns
  and **an unknown catalog column reads as NULL instead of raising**,
  which is why `IMPORT FOREIGN SCHEMA` silently imports nothing
  (`NOT c.relispartition` is NULL for every row).
- **One settings store** (Phase 1.5). Startup-packet `options` is read
  today only by two ad-hoc regexes (statement_timeout and
  `-c datestyle=`), so there is no general `-c name=value`. The store
  gives `postgres_fdw` sessions an `AS OF` through
  `OPTIONS (options '-c ...')`.

## Scope: two regression files are excluded by inertia

`scope.edn` puts `foreign_data` and `plpgsql` out of scope under
`:server-extension-hooks`. Both are stale against our own plan:
`foreign_data` is the natural gate for Option 2, and Phase 7.3 commits
to plpgsql. Move each in when its feature lands rather than leaving it
excluded.

## Staging

| Stage | What | Effort |
|---|---|---|
| **0** | Option 2: catalog columns, `regclass` OID consistency, streaming cursors + `MOVE BACKWARD ALL`, parenthesized joins with inner aliases, an EXPLAIN cost line, SQL `DEALLOCATE` of protocol-level statements. Gated by a local PG 17 + `postgres_fdw`, and by `foreign_data` | 1.5-2.5 wk |
| **0.5** | the three seams above | ~1 wk |
| **1** | Option 1 MVP: pgjdbc stream behind a `ChangeStream` protocol, snapshot via COPY into a staging branch, DDL through the SQL path, direct tx-data apply with tx-meta and the cursor entity, confirm-after-resolve, monotonic `txInstant`, read-only mirror, TOAST `'u'`, TRUNCATE, metrics, kill/restart harness | 4-6 wk |
| **2** | hardening: tablesync for tables added later, schema evolution from `Relation` diffs, auto re-snapshot on slot loss, PG 14-16, large-tx spill, optional native replication client | 3-5 wk |
| **3** | Option 3(b) a Debezium-shaped change stream, then maybe (a) a walsender | 2-3 wk / 6-10 wk |

## Known performance trap in Option 2, today

`DECLARE`/`FETCH`/`CLOSE` work, but each `FETCH` re-runs the stored
query with `LIMIT n OFFSET pos` (`rewrite-cursor-page`). At the default
`fetch_size` of 100 a 1M-row scan is 10k queries, each re-evaluating up
to its offset: **O(n²)**. This is not FDW-specific -- pgjdbc with a
fetch size, psycopg named cursors and plpgsql cursors all pay it.

It looked like it might also be a **correctness** bug, because paging by
`LIMIT/OFFSET` across re-executions is only sound under a *total* order
and the order injected for a cursor without `ORDER BY` is
`[1 :asc]` -- non-total as soon as the first column has duplicates.
**Measured, and it does not reproduce:** 24 rows with 6 per
first-column value, paged at 1, 3, 5 and 7, return every row exactly
once. Each `FETCH` re-executes against the same frozen
`:speculative-db` captured at `BEGIN`, and the executor is
deterministic over an immutable snapshot, so successive executions
agree on the tie order. `cursor-paging-does-not-repeat-or-skip-rows`
in `pg_cursor_protocol_test.clj` pins that property, so a future
paging or planner change cannot quietly take it away.

So this is a performance item, not an S1 -- which also means the fix is
free to change the strategy (hold an iterator, or materialize once per
cursor) without a correctness argument to preserve.

## Open questions

Answered: **startup `options`** is read by two regexes only, so FDW time
travel waits on the settings store.

Measurements, not opinions: are datahike branch create/reset/merge cheap
and atomic enough to stage a multi-GB snapshot; is a multi-million-datom
retraction for TRUNCATE acceptable or do we want a datahike primitive;
does the writer preserve `transact!` submission order (wanted as a test
either way).

Decisions: mirror as a component in the server process or a separate
writer (affects where pgjdbc lives, and whether it is a separate
artifact); read-only forever, or "mirror plus local tables" -- local
extras break the sole-writer argument, so v1 is read-only; one upstream
schema per mirror database, or the schema folded into the attribute
namespace (`:sales.orders/id`); per-row valid time from upstream
`valid_from`/`valid_to` columns, where datahike has tx-level
`:db.valid/from`/`to` and not per-row.
