# Mirroring a live PostgreSQL, with our semantics

**Two-table subscriber verified locally against PostgreSQL 18.6.**
The [prototype](../test/datahike/pg/mirror_prototype.clj) uses native
`pgoutput`, COPY and pg-datahike's existing SQL transaction path.
See the [roadmap](consolidation-plan.md) for productizing the subscriber.

## The goal

A read-only pg-datahike database mirrors a publication from an existing
PostgreSQL database. PostgreSQL remains the write endpoint. The mirror
loads a consistent snapshot, follows native logical replication, and
applies each upstream commit as one local transaction. Clients can query
it with SQL or Datalog and inspect history from the initial snapshot
onward with `FOR SYSTEM_TIME AS OF`.

## The three shapes, and which ones we want

| | What | Verdict |
|---|---|---|
| **Option 1** | upstream PostgreSQL → pg-datahike live mirror (CDC in) | **Next prototype after the compatibility batch.** |
| **Option 2** | a running PostgreSQL querying us through `postgres_fdw` / `dblink` | Independent interoperability work that shares catalog and test infrastructure. |
| **Option 3** | pg-datahike as a logical-replication publisher (CDC out) | **Defer.** Needs a durable commit cursor and consumer-pinned GC roots in datahike, after the 1.0 id layout |

## Native subscriber and apply path

Use pgJDBC to connect in replication mode to a stock PostgreSQL publisher.
Start with `pgoutput` protocol version 1 in text mode. Create a logical
slot with an exported snapshot, import it on a regular connection, and
COPY the published tables into a staging mirror. Catch up from the slot's
consistent position before making the mirror available to readers.
Configure the upstream connection, publication, and slot explicitly.
`CREATE SUBSCRIPTION` can come later as a SQL management interface.
[PostgreSQL replication protocol](https://www.postgresql.org/docs/18/protocol-logical-replication.html) · [Exported snapshots](https://www.postgresql.org/docs/18/logicaldecoding-explanation.html#LOGICALDECODING-SNAPSHOTS)

**Verified against PostgreSQL 18.6 with pgJDBC 42.7.3.** The slot snapshot
showed the original rows while writes committed. The stream delivered an
update, insert, and delete across two tables in one commit and omitted
a rolled-back insert. Receiving the commit and sending status feedback
left `confirmed_flush_lsn` at the slot's start position. Setting the
applied and flushed LSNs explicitly advanced it to the transaction's end
position. The prototype applies decoded rows and updates its checkpoint
in one local SQL transaction. The ordinary SQL commit uses
`transact-recorded!`.

Generate parameterized SQL from decoded row changes for the first apply
implementation. Keep one write path and one commit boundary. Use the
existing transaction machinery through `transact-recorded!`. Consider
direct datom generation if the prototype shows a measurable SQL cost or
uncovers apply semantics that the shared path cannot express.

- Resolve updates and deletes by the source key. Use the old key for a
  key-changing update. The destination already represents single and
  composite primary keys.
- Leave an unchanged TOAST column out of the update. Set a column to
  SQL NULL when the stream provides an explicit NULL. This allows tables
  with large values to use ordinary primary-key replica identity.
- Apply `TRUNCATE` to the published relations as part of the source
  transaction. With history enabled, reads before the truncate still see
  the original rows.

[PostgreSQL row change formats](https://www.postgresql.org/docs/18/protocol-logicalrep-message-formats.html)

## Apply and recovery guarantees

1. **Apply the upstream effects once.** Use an internal apply mode on the
   existing write path for source-computed values and effects. Keep
   destination storage invariants in force and define how apply handles
   transactions whose source constraints were deferred.
2. **Commit through `transact-recorded!`.** This keeps the conflict ring
   consistent. Pass transaction metadata through the same path when
   needed.
3. **Acknowledge only after a durable commit.** One worker owns feedback
   for the slot.
4. **Commit rows and the checkpoint together.** Persist the source
   transaction's end LSN in a cursor entity in the same transaction.
   Skip redeliveries at or below the recorded position. Record the
   upstream commit timestamp separately from Datahike's transaction time.
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

## Shared compatibility work

- **Catalog fidelity** (consolidation-plan Phase 4 and the `sysviews`
  cluster). Discover source columns and keys through PostgreSQL's
  catalogs. Load the mirror's schema through the existing DDL path so its
  SQL translator, catalogs, and `\d` agree.
- **FDW session settings** (Phase 1.5). A shared settings store lets
  `postgres_fdw` sessions select an `AS OF` through
  `OPTIONS (options '-c ...')`.

## Scope: two regression files are excluded by inertia

`scope.edn` puts `foreign_data` and `plpgsql` out of scope under
`:server-extension-hooks`. Both are stale against our own plan:
`foreign_data` is the natural gate for Option 2, and Phase 7.3 commits
to plpgsql. Move each in when its feature lands rather than leaving it
excluded.

## Subscriber milestones

- [x] Verify the native transport, slot snapshot, and explicit feedback.
- [x] Use a local REPL prototype to mirror
  two ordinary keyed tables. Keep writes going during COPY. Apply a
  multi-table transaction with a key change, a delete, and an update that
  leaves a large value unchanged. Kill the worker after the destination
  commit and before acknowledgement, then restart and compare current
  rows and historical reads. Verify one local transaction per applied
  source commit and confirm that a rolled-back source change stays absent.
- [ ] Turn the prototype into a read-only mirror with durable storage,
  staging and cutover, schema drift detection, `TRUNCATE`, and lag and
  checkpoint status. Run the focused PostgreSQL oracle during development
  and the full integration suites before opening the PR.
- [ ] Extend table synchronization and schema evolution, recovery after
  slot loss, failover handling, and support for large transactions.

### Running the prototype probe

Set `PG_MIRROR_SOURCE_URL` to a JDBC URL for an empty native PostgreSQL
18 database on a server with `wal_level=logical`. The connection needs
to create the test publication and replication slot. Run the probe from
the repository root after compiling the Java sources with `bb prep`.

```bash
clojure -M:dev -m datahike.pg.mirror-probe
```

The probe opens a file-backed pg-datahike store with history enabled.
While COPY reads the exported snapshot, the publisher commits an update
with an unchanged TOAST field, a primary-key change, an insert and a
delete. A separate process applies the commit and is halted before
acknowledgement. The probe reopens the store, verifies the duplicate
arrives without adding history, then follows another commit with an
explicit NULL. It compares current rows, the checkpoint, and historical
rows from before the first apply.

The prototype accepts whole-table publications with default primary-key
replica identity and supported primitive column types. Keep the
destination private to the worker while exercising the probe. A
database-enforced read-only endpoint, schema drift handling beyond
rejection, and transactional streaming for large commits are separate
subscriber milestones. The probe prints the local store and worker log
directory and uses a slot in the disposable source database.

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
