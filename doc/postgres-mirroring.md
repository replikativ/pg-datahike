# PostgreSQL logical replication into pg-datahike

The read-only subscriber mirrors a publication from a PostgreSQL 18 primary.
It takes a consistent initial snapshot and then follows committed transactions
over PostgreSQL's native logical replication protocol. The mirror keeps
Datahike history from the snapshot onward. PostgreSQL remains the write endpoint.

## Start a mirror

Enable `wal_level=logical` on the publisher and create a publication with the
tables to mirror.

```sql
CREATE PUBLICATION analytics FOR TABLE public.accounts, public.events;
```

Use a publisher role with `LOGIN`, `REPLICATION`, database access, and
`SELECT` on all published tables. The role needs access to
`pg_logical_emit_message(boolean, text, text, boolean)` for catch-up barriers.
Connect to the publisher primary.

Save a configuration as `mirror.edn`.

```clojure
{:publication "analytics"
 :store-path "/var/lib/pg-datahike/analytics"
 :server {:port 15432 :default "analytics"}}
```

```bash
export PG_MIRROR_SOURCE_URL='jdbc:postgresql://publisher.example/production?user=mirror&sslmode=verify-full'
java -jar pg-datahike-standalone.jar mirror mirror.edn
```

From a source checkout, use `clojure -M -m datahike.pg.main mirror mirror.edn`
after `bb prep`. Embedded callers use `datahike.pg.mirror/start!` with the
same configuration plus `:source-url`, then use `status` and `stop!`.
The listener uses the ordinary pgwire authentication and TLS options.

Use a dedicated store path. Keep its adjacent `.mirror-lock` file with the
store. One worker owns the store and a generated permanent `pgoutput` slot.
The SQL listener opens after snapshot copy and catch-up. Client writes return
SQLSTATE `25006`.

## Check a publication

Use the same source URL and publication as the mirror configuration to inspect
compatibility before creating a replication slot or a local store.

```bash
java -jar pg-datahike-standalone.jar mirror --check mirror.edn
```

The command prints an EDN report with publication-wide issues and the
specific rules and columns that block each table. It exits with status 1 for
an incompatible publication. Embedded callers can use
`datahike.pg.mirror/publication-report` with `:source-url` and `:publication`.

## Supported publications

This version accepts whole, unfiltered, permanent tables with a single or
composite non-deferrable primary key and default replica identity. It copies column names,
supported types, and primary keys into a fresh read model. Publisher defaults,
triggers and cascades run on PostgreSQL. The mirror applies the resulting rows
and preserves their transaction boundaries using the normal Datahike writer.
Generated columns, sequence counters, legacy inheritance, row filters, and
column projections require separate handling.

Supported column types are boolean, smallint, integer, bigint, real, double
precision, numeric, text, varchar, character, bytea, uuid, date, time,
timetz, timestamp, timestamptz, json, jsonb, text[], and tsvector.
The subscriber preserves character widths, numeric precision and scale,
and time and timestamp precision. Snapshot COPY and live apply preserve
PostgreSQL microseconds, `24:00:00`, `timetz` offsets, and `text[]`
lower bounds. Array support in the subscriber currently covers `text[]`.

The subscriber also copies enums in `public` and domains in `public`
over supported scalar built-in types without base type modifiers.
Domains can have a default, `NOT NULL`, and one validated `CHECK`.
The admission report identifies unsupported type definitions by column.
The mirror installs type definitions with the tables or columns that use
them. With schema refresh enabled, it follows enum label additions and
renames and changes to a domain's default, nullability, and check.
`pg_enum` reports the publisher's member OIDs and sort order.

## Mirror partitions

Use the publisher's default leaf routing, with
`publish_via_partition_root = false`, for declarative partitions in
`public`. The mirror copies and applies changes to each published leaf.
When the publication covers all leaves, readers can query the parent or
any intermediate parent. `ONLY` reads the empty physical parent.
`pg_class` and `pg_inherits` expose the mirrored table hierarchy.
`pg_partitioned_table` reports partition keys and default partitions
using the mirror's relation OIDs and column numbers. `pg_get_partkeydef`
renders the key definition, and `pg_get_expr` renders partition bounds
from `pg_class.relpartbound`. On a parent scan, `tableoid::regclass`
identifies the leaf that supplied the row.

A new attachment takes a consistent snapshot of the affected leaves.
The new parent layout and its rows become visible at the same durable
WAL boundary. A detached leaf disappears from current reads when it
leaves the publication. Historical reads preserve its earlier rows,
partition keys, and bounds.

The admission report flags root routing because direct leaf truncations
are absent from its WAL stream. A pending detach entry in the publisher's
catalog pauses the mirror until the catalog has settled and the worker
restarts.

## Add tables to an active mirror

Add supported tables to the publisher's publication. The worker detects the
addition and begins copying the new tables while continuing to apply changes
to the existing tables.

```sql
ALTER PUBLICATION analytics ADD TABLE public.activity_log;
```

Refresh uses an additional temporary logical slot on the publisher and two
additional connections while it runs. Status reports the tables and the
copying or catch-up phase under `:refresh`. From a shared WAL commit, the
mirror installs the new tables and records the refreshed publication and
checkpoint in one durable transaction. The new tables become queryable at
that commit, which is where their local history begins.

If the refresh stops before installation, a restart stages the new tables
from a fresh snapshot. A restart after installation resumes from the durable
checkpoint. The temporary slot is released when its connection closes.
The snapshot byte limit also bounds staging input, including replayed data.

Removing published tables pauses the mirror.

## Refresh after a publisher schema change

Set `:schema-refresh? true` in the mirror configuration to support adding,
renaming, and dropping columns, changing supported types and precision, and
changing defaults or nullability. It also follows table renames within the
publisher schema when the table keeps its PostgreSQL relation OID. Group
related DDL in one PostgreSQL transaction and allow each refresh to finish
before the next schema change.
Keep this option enabled through restart while a refresh is pending.

Columns can change between supported built-in types, enums, and domains if
the previous primary key can still match rows. The mirror matches renamed
enum key labels by the publisher's enum member OIDs. Earlier snapshots
retain the old names, column types, defaults, and checks. Schema refresh
requires the same PostgreSQL type identity for edits to registered types.

When upgrading an existing mirror that stored only the wire layout, enable
this option while its publication still has the same layout. Let the mirror
finish recording the source column numbers and definitions before migrating.
An upgrade from an older enum manifest records the member OIDs at its next
schema refresh. Let that refresh finish before renaming an enum label.

The worker uses PostgreSQL's post-migration rows at the shared WAL boundary.
The refreshed schema and rows become visible with the checkpoint. Existing
Datahike entities keep their identity when their primary keys match, and
historical SQL reads retain the old columns and values. Source default
expressions are recorded as catalog metadata. PostgreSQL evaluates them
when applications write to the publisher.

A table rename preserves the mirror's relation OID and Datahike entities.
The new SQL name becomes visible at cutover. Earlier snapshots keep the
old name, including when the publisher adds a new table under that name.

The mirror follows changes to a primary key's columns and their order.
It matches rows by the previous key during refresh and uses the new key
for WAL changes after cutover. Keep the previous key columns, their types,
and their non-null, unique values through cutover. The mirror pauses if
the previous key columns are missing or retyped, or their values make the
row match ambiguous. A later migration can remove the previous columns.

The affected tables have a resnapshot window. Their publisher changes during
that window are reflected in the cutover transaction. Status and the durable
manifest report its start positions and cutover LSN under
`:last-schema-refresh`. The manifest also records tables awaiting refresh,
so a restart resnapshots them even when the source layout has changed back.
With schema refresh disabled, schema drift pauses the mirror.

Use `SET datahike.as_of` to query the pre-refresh columns and rows. The
same snapshot applies to `information_schema.columns`, `pg_attribute`,
and catalog joins. `FOR SYSTEM_TIME AS OF` selects a snapshot for one
statement and works with prepared queries.

## Recovery and status

The checkpoint in `pgdh_mirror.checkpoint` stores the publisher identity, end
LSN, transaction ID, and publisher commit timestamp. Each source commit and its
checkpoint reach durable storage together. Feedback reports committed positions
only. After a disconnect, commits already in the checkpoint are acknowledged
and skipped.

Status includes the applied and confirmed LSNs, retained WAL, and the most recent
error. A paused worker that has already cut over serves the last committed
state. When a connection drops, the worker reconnects with backoff and catches
up from its durable checkpoint. A missing slot, an invalidated slot, or a
confirmation ahead of the checkpoint pauses the mirror by default.

Snapshot and transaction sizes are bounded by `:max-snapshot-bytes` and
`:max-transaction-bytes`. The defaults are 128 MiB and 64 MiB respectively.
The worker pauses when it hits a limit. Stop the worker and adjust the limit to
retry. Cutover and occasional idle WAL confirmation use transactional logical
messages. Their checkpoint transactions can appear in Datahike history.

The mirror uses pgoutput streaming for large transactions. It keeps their
encoded changes in `<store-path>.mirror-spool` until commit or abort. Main
and schema refresh streams share the `:max-stream-spool-bytes` allowance,
which defaults to 1 GiB. Status reports the current buffered bytes under
`:spooled-bytes`. The transaction limit also applies to each streamed
transaction. Applying a committed stream uses one Datahike transaction,
whose local write set still takes memory proportional to the changes.

Savepoint rollbacks discard the affected portion of a spool. A restart
clears stale spool files and replays from the durable checkpoint. WAL
feedback advances only after the local commit is durable.

Stopping a subscriber leaves its slot available for resume. After permanently
retiring a mirror, drop its recorded slot on the publisher to release retained
WAL once the worker has stopped.

## Resnapshot after slot loss

Set `:resnapshot-on-slot-loss? true` to recover from a missing or invalidated
slot or a confirmation ahead of the durable checkpoint. The worker checks
the publisher identity and publication, reserves a replacement slot, and
copies a consistent snapshot. It installs the snapshot, new slot name, and
WAL gap record together. If the original slot is present, allow room for
one more logical slot. The worker retires the original after cutover when
the slot still matches the mirror's configuration and is inactive.

An open listener serves the last durable snapshot while the replacement
copies. Earlier rows and catalog definitions remain queryable with
`datahike.as_of`. The recovery records the interval it replaced. It keeps
matching entities by their previous keys and uses the ordinary schema
refresh rules for publisher DDL.

Status reports the copy or install phase under `:recovery` and the latest
completed interval under `:last-recovery`. The checkpoint manifest keeps
each interval under `:recovery-gaps`, including the old and new slot names,
timelines, the last durable LSN, and the replacement snapshot LSN. Keep
the option enabled through restart while recovery is pending. A restart
before install copies again using the reserved slot name. A restart after
install follows the replacement slot and retries any pending slot retirement.

## Follow a promoted PostgreSQL standby

Set `:failover? true` to create a permanent logical slot with `FAILOVER`
enabled. Configure a physical standby with slot synchronization and make
the publisher wait for the standby's physical slot through
`synchronized_standby_slots`. Verify that the standby has synchronized the
mirror's slot before promotion. The PostgreSQL failover procedure describes
these publisher settings. [PostgreSQL logical replication failover](https://www.postgresql.org/docs/18/logical-replication-failover.html)

Supply a primary-aware endpoint or a multi-host JDBC URL so the mirror can
find the promoted primary. The mirror selects a primary for both logical
replication and SQL, and checks that the connections reached the same instance.

The promoted primary must retain the same synchronized slot. The mirror
checks the database identity and timeline history, and requires the old
timeline to contain the durable checkpoint before the fork. It replays
from the synchronized slot, skips already committed positions, and records
the new timeline with the first durable commit before reporting feedback.
Status reports the timeline under `:source-timeline`. A pending schema
refresh starts its staging snapshot again on the promoted primary.

To enable failover on an existing mirror, stop it and use
`ALTER_REPLICATION_SLOT slot_name (FAILOVER true)` over a PostgreSQL replication
connection, then restart with `:failover? true`. When failover is disabled, a
timeline change pauses the mirror. A lost or invalidated slot, or a timeline
that forked before the checkpoint, requires a new snapshot. For a fork
before the checkpoint, preserve the old store and bootstrap at a fresh path.

## Verification and follow-up

The [replication probe](../test/datahike/pg/mirror_probe.clj) compares the
mirror with a stock PostgreSQL subscriber. It uses two disposable PostgreSQL
18 databases, a source and native reference subscriber. Set
`PG_MIRROR_SOURCE_URL` and `PG_MIRROR_REFERENCE_URL` to the JDBC URLs and
`PG_MIRROR_SOURCE_CONNINFO` to the source libpq connection string used by
`CREATE SUBSCRIPTION`. The source server needs `wal_level=logical`. Run
`clojure -M:dev -m datahike.pg.mirror-probe` after `bb prep`. Pass
`--refresh` to exercise online table addition and its recovery boundaries.
Pass `--schema-refresh` to exercise publisher DDL, table name reuse, restart,
and historical row and catalog reads. Pass `--primary-key-refresh` to compare
replacement keys, key order, historical constraints, and rejected cutovers
with PostgreSQL. Pass `--partitions` to check populated attachment,
detachment, reattachment, truncation and crash recovery under leaf routing.
Pass `--type-refresh` to compare enum and domain changes, column conversions,
and historical type catalogs across an interrupted cutover and restart.
Run `sh test/integration/postgres-regress/mirror-failover.sh promote` with
`PG_MIRROR_PG_BIN` pointing to PostgreSQL 18 binaries to promote a disposable
standby during a schema refresh. The `diverge` mode promotes a standby that
forked before the checkpoint and verifies that the mirror pauses.
Run `sh test/integration/postgres-regress/mirror-slot-recovery.sh` to check
recovery from dropped and invalidated slots, recovery while serving reads,
and the snapshot cutover before and after a crash.
Run `sh test/integration/postgres-regress/mirror-publisher.sh --streaming`
to verify interleaving, savepoint rollback, streamed schema catch-up,
and replay after a spool limit or a crash.

The next stages cover more publication types and relations. See the
[consolidation plan](consolidation-plan.md) and
[PostgreSQL logical replication restrictions](https://www.postgresql.org/docs/18/logical-replication-restrictions.html).

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

The next subscriber stages need measurements of staging large tables and
retracting large relation sets on TRUNCATE. Mapping upstream `valid_from`
and `valid_to` values to Datahike's transaction-level valid time also needs
a separate design. FDW time travel depends on support for startup
`options` in the session settings.
