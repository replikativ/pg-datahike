# PostgreSQL logical replication into pg-datahike

The read-only subscriber mirrors a publication from a PostgreSQL 18 primary.
It takes a consistent initial snapshot and then follows committed transactions
over PostgreSQL's native logical replication protocol. The mirror keeps
Datahike history from the snapshot onward. PostgreSQL remains the write endpoint.

History is dated by the publisher. Each mirrored transaction is recorded at
its PostgreSQL commit timestamp, in milliseconds, and the initial snapshot at
the publisher's clock just after the snapshot was taken. `SET datahike.as_of` and
`FOR SYSTEM_TIME AS OF` therefore select by the time the application
committed, whatever the replication lag was. PostgreSQL does not guarantee
that commit timestamps rise in commit order. A transaction whose timestamp
is earlier than its predecessor's is recorded at the predecessor's instant,
so outside a table or schema refresh a point in time selects a prefix of
the publisher's commits. Transactions recorded by a release before this change keep the mirror's
clock.

Three kinds of transaction are not publisher commits. The mirror's own
bookkeeping, such as the transaction a restart commits when it opens the
store, is dated by the mirror's clock and changes no mirrored row. A schema
or table refresh becomes visible at the publisher commit that completes its
cutover, which is later than the original DDL. Changes to a table under
refresh are held back until that cutover while other tables keep applying,
so a time inside the refresh window can show a later commit to another table
without an earlier commit to the refreshed one, and one publisher transaction
that touched both appears at two instants. A resnapshot after slot loss
is dated by the mirror's clock, or one millisecond after the last applied
commit when that is later, and the commits that follow it share that instant
until the publisher's commit times pass it.

A time selects whole transactions. `SET datahike.as_of` also accepts a
Datahike transaction number, and the batches of the initial snapshot are
separate transactions: a number inside the snapshot selects the tables as
far as they had been copied. Use a time to read the snapshot.

## Start a mirror

On the PostgreSQL primary, set `wal_level = logical` and restart PostgreSQL
if the setting changes. Leave room in `max_replication_slots` and
`max_wal_senders` for the mirror. A schema refresh uses an additional
logical slot while it stages its snapshot.

In the source database, create a publication and grant a dedicated login
access to the published tables. This example uses the database `production`.
Run `CREATE PUBLICATION` as a role that owns the tables and can create
publications. Run the role setup as a database administrator.

```sql
CREATE PUBLICATION analytics FOR TABLE billing.accounts, archive.events;

CREATE ROLE mirror LOGIN REPLICATION;
GRANT CONNECT ON DATABASE production TO mirror;
GRANT USAGE ON SCHEMA billing, archive TO mirror;
GRANT SELECT ON billing.accounts, archive.events TO mirror;
GRANT EXECUTE ON FUNCTION pg_catalog.pg_logical_emit_message(boolean, text, text, boolean) TO mirror;
```

Configure authentication for that role in `pg_hba.conf` using the source
database name. Supply the publisher credentials to the mirror process via
its JDBC connection settings.

Save a configuration as `mirror.edn`.

```clojure
{:publication "analytics"
 :store-path "/var/lib/pg-datahike/analytics"
 :server {:port 15432 :default "analytics"}}
```

```bash
export PG_MIRROR_SOURCE_URL='jdbc:postgresql://publisher.example/production?user=mirror&sslmode=verify-full'
java -jar pg-datahike-standalone.jar mirror --check mirror.edn
java -jar pg-datahike-standalone.jar mirror mirror.edn
```

From a source checkout, use `clojure -M -m datahike.pg.main mirror mirror.edn`
after `bb prep`. Embedded callers use `datahike.pg.mirror/start!` with the
same configuration plus `:source-url`, then use `status` and `stop!`.
The listener uses the ordinary pgwire authentication and TLS options.

Use a dedicated store path. Keep its adjacent `.mirror-lock` file with the
store. One worker owns the store and a generated permanent `pgoutput` slot.
The SQL listener opens after snapshot copy and catch-up. Client writes return
SQLSTATE `25006`. Verify a representative read and compare it with the
publisher while writes are quiet.

```bash
psql -X --host=127.0.0.1 --port=15432 --username=datahike \
  --dbname=analytics --command='SELECT count(*) FROM billing.accounts;'
```

## Group commit for ordinary row changes

On a build with Datahike ordered transaction preparation, add
`:max-in-flight-commits 8` to the mirror configuration to try queued replay.
The default is one, and the experimental queue accepts depths up to 32.
Status reports `:queued-replay :enabled`. A publisher constraint change that
the queue cannot follow switches it off until the worker restarts, and status
then reports `:queued-replay :suppressed`.
It holds at most 8 MiB of queued source changes. The mirror opens the
store with an exclusive Datahike writer. Keep independent Datahike writers
off this store while the mirror owns it.

Each publisher commit keeps a distinct Datahike transaction and historical
version. Several of those transactions can share one physical storage commit.
Feedback advances through the durable prefix in source order. The mirror
drains queued writes before schema cutovers, streamed transactions, and
other changes handled by the serial replay path.

## Check a publication

Use the same source URL and publication as the mirror configuration to inspect
compatibility before creating a replication slot or a local store.

```bash
java -jar pg-datahike-standalone.jar mirror --check mirror.edn
```

The command prints an EDN report with publication-wide issues and the
specific rules and columns that block each table. It exits with status 1 for
an incompatible publication. Embedded callers can use
`datahike.pg.mirror/publication-report` with the mirror's source and
publication options.

## Supported publications

The mirror accepts whole, unfiltered, permanent tables in user schemas.
Readers can query them by their qualified names. Tables and indexes with
the same name in different schemas remain separate. Default replica
identity requires a single or composite non-deferrable primary key. Under
`REPLICA IDENTITY FULL`, the mirror uses that key when available. Otherwise
it matches the entire old tuple, including NULL values, and applies the
change to the matching row with the lowest local entity ID. The current
rows preserve duplicate counts. Historical entity IDs reflect the mirror's
choice among identical rows. Whole-row matching requires equality on every
published column and deterministic collations. The admission report flags
unsupported types such as `json` and `json[]` for those tables.

`REPLICA IDENTITY USING INDEX` supports tables with or without a primary
key. The selected index must be an immediate unique B-tree index over
non-null columns with default operator classes and deterministic collations.
The mirror uses the selected key columns for updates and deletes. It copies
those columns as a unique index and preserves the publisher's immediate
primary key separately when one exists. Index `INCLUDE` columns are copied
as table columns.

The mirror copies column names and supported types into a fresh read model.
Publisher defaults,
triggers and cascades run on PostgreSQL. The mirror applies the resulting rows
and preserves their transaction boundaries using the normal Datahike writer.
The mirror supports stored generated columns when the publication includes
them. For whole-table publications, set `publish_generated_columns = stored`
on PostgreSQL. The publisher computes the values, and the mirror records
their expression and generated status for catalog queries. Virtual generated
columns, sequence counters, legacy inheritance, row filters, and partial
column lists require separate handling.

Supported column types are boolean, smallint, integer, bigint, real, double
precision, numeric, text, varchar, character, bytea, uuid, date, time,
timetz, timestamp, timestamptz, json, jsonb, and tsvector. The mirror also
supports arrays of these types, with element typmods, value dimensions,
and lower bounds.
The subscriber preserves character widths, numeric precision and scale,
and time and timestamp precision. Snapshot COPY and live apply preserve
PostgreSQL microseconds, `24:00:00`, and `timetz` offsets.

The subscriber also copies enums in `public` and domains in `public`
over supported scalar built-in types without base type modifiers.
Domains can have a default, `NOT NULL`, and one validated `CHECK`.
The admission report identifies unsupported type definitions by column,
including checks that depend on publisher-side functions or operators
outside `pg_catalog`.
The mirror installs type definitions with the tables or columns that use
them. With schema refresh enabled, it follows enum label additions and
renames and changes to a domain's default, nullability, and check.
`pg_enum` reports the publisher's member OIDs and sort order.

## Mirror partitions

Use the publisher's default leaf routing, with
`publish_via_partition_root = false`, for declarative partitions.
A partition tree can span multiple schemas. The mirror copies and applies
changes to each published leaf.
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

A pending detach entry in the publisher's catalog pauses the mirror until
the catalog has settled and the worker restarts.

### Mirror alongside a root-routed publication

PostgreSQL sends changes using the root relation identity and omits direct
leaf truncations when `publish_via_partition_root` is enabled. To preserve
the source rows and leaf identities, give the mirror its own leaf-routed
publication alongside the root publication used by existing subscribers.

For example, if `analytics` publishes `public.orders` and `public.customers`
with root routing, create the leaf publication on the same publisher.

```sql
CREATE PUBLICATION analytics_leaf FOR TABLE public.orders, public.customers
  WITH (publish_via_partition_root = false);
```

Configure a fresh mirror with `:publication "analytics"` and
`:leaf-publication "analytics_leaf"`. Both publications must use the same
selectors, such as the same list of tables, the same list of schemas, or
`FOR ALL TABLES`. The mirror checks that both publish the same operations
and that neither has row filters or column lists. Matching selectors
also include future partitions as they attach. The mirror reads
`pgoutput` from `analytics_leaf`, and the checkpoint binds both publication
identities to the store.

Edit both definitions in one publisher transaction when changing explicit
membership or publication options. The mirror checks the pair during its
catalog polls, before snapshot copy, and at cutover. It pauses when it
detects a mismatch. If the leaf publication was edited separately and could
have excluded writes, create a fresh mirror to establish a consistent
baseline. Publication changes can take effect between catalog polls.

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

When an ordinary table leaves the publication, the mirror keeps its last
applied rows queryable under its qualified name. A same-named table in
another schema continues to apply changes independently. If the publisher
reuses the qualified name for a different table, the new table receives
its own local identity. Earlier snapshots retain the old relation. If only
an index name is reused, the frozen table remains readable and releases
that index from its current catalog. Earlier snapshots retain the index.
Republishing the frozen table installs its current publisher keys at
cutover and resumes WAL apply.

## Refresh after a publisher schema change

Set `:schema-refresh? true` in the mirror configuration to support adding,
renaming, and dropping columns, changing supported types and precision, and
changing defaults or nullability. It also follows table renames and schema
moves when the table keeps its PostgreSQL relation OID. Group
related DDL in one PostgreSQL transaction and allow each refresh to finish
before the next schema change.
Keep this option enabled through restart while a refresh is pending.

Columns can change between supported built-in types, enums, and domains.
On keyed tables, the previous key must still match rows. The mirror matches
renamed enum key labels by the publisher's enum member OIDs. Renaming a tracked
enum or domain preserves its mirror type OID. When the publisher drops
one and reuses its name, the replacement receives a new mirror type OID.
Earlier snapshots retain the previous names, types, defaults, and checks.

When upgrading an existing mirror that stored only the wire layout, enable
this option while its publication still has the same layout. Let the mirror
finish recording the source column numbers and definitions before migrating.
An upgrade from an older enum manifest records the member OIDs at its next
schema refresh. Let that refresh finish before renaming an enum label.

The worker uses PostgreSQL's post-migration rows at the shared WAL boundary.
The refreshed schema and rows become visible with the checkpoint. Existing
Datahike entities keep their identity when their previous keys match, and
historical SQL reads retain the old columns and values. Source default
expressions are recorded as catalog metadata. PostgreSQL evaluates them
when applications write to the publisher.

Schema refresh follows the addition, removal, and expression changes of
published stored generated columns. It preserves the generated status and
expression in earlier snapshots.

A table rename or schema move preserves the mirror's relation OID and
Datahike entities. The new qualified name becomes visible at cutover.
Earlier snapshots keep the old name, including when the publisher adds a
new table under that name. When a publisher renames a whole schema, the
mirror moves the published tables and can retain the earlier namespace.

The mirror follows changes to the primary key and changes to the selected
replica identity. During refresh, it matches rows by a retained primary key
or the previous identity index. It can also use the previous primary key
when its columns survive a key change. After cutover, it uses the new
replica identity for WAL changes. On keyed tables, keep the matching
columns, their types, and their non-null, unique values through cutover.
The mirror pauses if it cannot establish a row match. A later migration can
remove the previous columns.

When either layout uses `FULL` without a non-deferrable primary key, refresh
pairs matching rows one at a time. It uses the previous key if it survives,
or the columns with the same publisher column numbers and types in both
layouts. This preserves IDs for matched duplicates, and new rows receive
new IDs. When a rewrite leaves no comparable columns, all current rows
receive new IDs. The earlier rows remain available in historical queries.

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

The internal Datahike checkpoint stores the publisher identity, end
LSN, transaction ID, and publisher commit timestamp. Each source commit and its
checkpoint reach durable storage together. Feedback reports committed positions
only. After a disconnect, commits already in the checkpoint are acknowledged
and skipped.

Status includes the applied and confirmed LSNs, retained WAL, and the most recent
error. A paused worker that has already cut over serves the last committed
state. When a connection drops, the worker reconnects with backoff and catches
up from its durable checkpoint. A missing slot, an invalidated slot, or a
confirmation ahead of the checkpoint pauses the mirror by default.

The initial catch-up and a table refresh may take as long as they need while
they keep applying commits. If nothing is applied for ten minutes, the worker
reports `:reason :replication-stalled`, reconnects, and continues from its
durable checkpoint. Data that is still arriving counts as progress;
keepalives do not. The rule watches the stream: a local write that never
completes is not detected by it, and neither is a refresh whose cutover
barrier the publisher holds back, for example while it waits for a
synchronized standby.

Do not `TRUNCATE` a published table while the initial snapshot is being
copied. PostgreSQL's `TRUNCATE` is not MVCC-safe: a table truncated before
the copy reaches it reads as empty, and the mirror's history then shows it
empty from the snapshot instant on. Current rows still converge.
A stall that persists keeps the worker reconnecting with that reason in its
status. It does not pause. A JVM error that ends the worker, such as an exhausted heap,
is reported as state `:failed`, and the CLI exits with status 1. An error
raised inside a local SQL statement, including an exhausted heap during a
COPY, surfaces as a failed statement and pauses the worker instead.

The initial snapshot is committed in batches of about 16 MiB of COPY data,
so the copy does not have to fit in memory, and it has no size limit unless
`:max-snapshot-bytes` is configured. A table with a composite primary key is
still checked for uniqueness in memory, during the copy, in the last
snapshot transaction, and when the listener opens, so a very large one can
need more heap than its batches. The checkpoint receives its position
with the last batch, after the worker has checked that the slot is still
usable. A slot the publisher invalidates in the moment between that check
and the first replication connection is handled like any later slot loss. If the snapshot stops before that last transaction begins, the worker
drops its slot on the publisher when it still can (a lost connection, a
killed process, or a failure inside the last transaction leaves the slot
until the next start), and the next attempt discards the
partial store and copies again. While a store is being replaced, the file
`<store-path>.mirror-reset` holds the slot name; keep it with the store. Every batch is recorded at the same instant,
so no point in time selects part of a snapshot.

The snapshot of a table or schema refresh, and a resnapshot after slot loss,
are staged in memory and bounded by `:max-snapshot-bytes`, 128 MiB by
default. Transaction size is bounded by `:max-transaction-bytes`, 64 MiB by
default.
The worker checks ordinary row frames before decoding their fields. pgJDBC
holds one complete wire frame in memory. The worker pauses when it hits a
limit. Stop the worker and adjust the limit to retry. Cutover and occasional idle WAL confirmation use transactional logical
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
and historical row and catalog reads. Run
`test/integration/postgres-regress/mirror-publisher.sh --namespaces` to
exercise same-named tables and indexes, cross-schema moves, unpublished
tables, and partitions on a disposable publisher. Pass `--frozen-index-names`
to the same script to verify index name transfer, historical reads after
restart, and key restoration when the frozen tables rejoin the publication. Pass `--primary-key-refresh` to compare
replacement keys, key order, historical constraints, and rejected cutovers
with PostgreSQL. Pass `--full-identity` to run the key changes with
`REPLICA IDENTITY FULL`, including mode changes, key-changing updates,
unchanged toasted values, and restart. Pass `--full-identity-recovery` to
exercise the snapshot and crash-replay probe with keyed `FULL` tables.
Pass `--keyless-full` to compare duplicate-row apply, NULL and toasted values,
schema refresh, primary-key transitions, and crash replay for keyless tables.
Pass `--index-identity` to exercise index-based apply, index changes, and
restart with a keyed table and a keyless join table.
Pass `--array-refresh` to exercise adding and removing columns, changing an
array element typmod, and matching rows by equivalent array keys at cutover.
Pass `--generated-columns` to compare stored generated values with a native
subscriber through COPY, live apply, expression changes, and restart.
Pass `--partitions` to check populated attachment, detachment, reattachment,
truncation and crash recovery under leaf routing.
Pass `--root-publication` to run the same comparison with a matched
leaf publication and verify admission and resume checks for the pair.
Pass `--type-refresh` to compare enum and domain changes, column conversions,
and historical type catalogs across an interrupted cutover and restart.
Run `sh test/integration/postgres-regress/mirror-publisher.sh --type-lifecycle`
to verify type renames, name swaps, drops and replacements, and preservation
of earlier type identities across a crash.
Run `sh test/integration/postgres-regress/mirror-failover.sh promote` with
`PG_MIRROR_PG_BIN` pointing to PostgreSQL 18 binaries to promote a disposable
standby during a schema refresh. The `diverge` mode promotes a standby that
forked before the checkpoint and verifies that the mirror pauses.
Run `sh test/integration/postgres-regress/mirror-slot-recovery.sh` to check
recovery from dropped and invalidated slots, recovery while serving reads,
and the snapshot cutover before and after a crash.
Run `sh test/integration/postgres-regress/mirror-publisher.sh --queued-crash`
to kill a mirror with queued commits in flight, and again after commits are
durable and before their feedback. It checks that the publisher's confirmed
position never passes the durable checkpoint and that a restart applies every
publisher commit exactly once. Set `PG_MIRROR_PROBE_QUEUE_DEPTH` to run any
other probe with queued replay.
Run `sh test/integration/postgres-regress/mirror-publisher.sh --streaming`
to verify interleaving, savepoint rollback, streamed schema catch-up,
and replay after a spool limit or a crash.

The next stages cover more publication types and relations. See the
[consolidation plan](consolidation-plan.md) and
[PostgreSQL logical replication restrictions](https://www.postgresql.org/docs/18/logical-replication-restrictions.html).

## Open questions

The next subscriber stages need measurements of staging large tables and
retracting large relation sets on TRUNCATE. Mapping upstream `valid_from`
and `valid_to` values to Datahike's transaction-level valid time also needs
a separate design.
