# Implicit-transaction unification

## Goal

Match PostgreSQL: **every message group that is not already inside an
explicit `BEGIN` block is one implicit transaction.** A group is

- a multi-statement simple `Q`, or
- a run of extended-protocol messages between two `Sync`s (this is how
  asyncpg's `executemany` and pgjdbc/node batches pipeline).

The group commits at its boundary (end-of-`Q` / `Sync`); any statement
error or an explicit `ROLLBACK` rolls back the **whole** group; and
statements in the group see each other's uncommitted writes
(read-after-write).

## What we had (pre-refactor) — three write paths, only one a real txn

1. **Explicit transactions** — `tx-state` speculative-db + `:tx-buffer`.
   `BEGIN` opens it, in-tx `exec-insert/update/delete/ddl` accumulate
   against `:speculative-db`, `COMMIT` transacts the buffer once,
   `ROLLBACK` discards. Correct; DML+DDL rollback + read-after-write all
   work. **Untouched by this refactor.**

2. **Extended autocommit** — `extBatch` held-bytes + `batch-state`.
   `beginBatchScope` activates; `exec-batchable-insert` holds INSERT
   tx-data + CC bytes; commits the held batch at `Sync` **and early at
   the first non-batchable statement**. Only INSERTs. The early commit
   means a mid-group `ROLLBACK`/read/error couldn't roll the group back
   atomically.

3. **Simple-`Q` autocommit** — `BatchBuffer`. Same idea; held INSERTs
   pre-flushed before a non-INSERT; end-of-`Q` flush; `errored → clear`
   (held INSERTs roll back, but anything already flushed does not).

Paths 2 and 3 are bulk-INSERT *performance* optimizations, not a
transaction model. They commit-once-at-boundary (good for perf) but
close the implicit transaction too early on any non-INSERT boundary.

## The model (post-refactor)

Reuse path 1 for the implicit group:

- A **write** (`INSERT/UPDATE/DELETE/DDL`) executed while **not** in an
  explicit tx opens an *implicit* transaction in `tx-state`
  (`:in-tx? true :implicit? true`, `:speculative-db` seeded). Subsequent
  statements in the group run in it via the existing in-tx branches;
  reads see `:speculative-db` (the dispatch already selects it when
  `:in-tx?`).
- The wire layer commits the implicit tx at the group boundary —
  `handleSync` (extended) and end-of-`Q` (simple) call
  `commitImplicit`, which transacts `:tx-buffer` once and resets
  `tx-state`. If the group was aborted (statement error set
  `:aborted?`) it discards instead. `:implicit? false` (explicit tx) →
  no-op, the explicit tx survives across Syncs until explicit `COMMIT`.
- An explicit `ROLLBACK` discards the implicit tx (existing
  `handle-rollback`); an explicit `COMMIT` flushes it.
- **CommandComplete is emitted per statement immediately** — PG does the
  same; the group is atomic, so a later error rolls back the already-
  acknowledged statements (clients expect this within a group). This
  removes the `extBatch`/`BatchBuffer` held-bytes machinery.

## Staging

- **Stage 1** (task #24): wire `commitImplicit` at `Sync`; route
  extended-protocol writes through the implicit tx; disable the
  batchable INSERT path. Validate asyncpg (executemany incl. RETURNING,
  atomicity, client-abort) + node-postgres.
- **Stage 2** (task #25): wire `commitImplicit` at end-of-`Q`; make
  multi-statement `Q` atomic. Validate Odoo/Metabase init, pg_dump
  replay, sqllogictest, pgjdbc.
- **Stage 3** (task #26): retire `extBatch`/`BatchBuffer`/
  `beginBatchScope`/`exec-batchable-insert`/`withBatchable`; benchmark
  bulk INSERT against pre-refactor numbers.

## Results (2026-06-08)

All three stages landed. Correctness: executemany fully correct (was
collapsing to 1 row), multi-statement `Q` atomic, client-abort rolls back
via the implicit `ROLLBACK` (no socket-backpressure hack). Zero
regressions across node-postgres (14/0/8), sqllogictest (61/0), and the
asyncpg suite (transaction 6/6, record/types/codecs at baseline).

**Performance — no regression.** An initial single-run asyncpg number
suggested ~30% slower single-row INSERT, but that was cold-JVM/GC/network
noise. Two stable measurements:

- Server-side micro-benchmark (no network), 3000 INSERTs after warmup:
  the implicit-tx path (`executeInGroup` + `commitImplicit`) ran at
  ~718 ops/s vs ~635 ops/s for a direct auto-commit — i.e. implicit-tx is
  *faster*, because it's the same `dc/with` + one `d/transact` the old
  batchable path did, with cheaper bookkeeping.
- asyncpg single-row INSERT (network-bound, warmed): 332–456 ops/s,
  at/above the pre-refactor single-run baseline (326). executemany now
  commits all rows at ~2.8–4.1 k/s (HEAD was *broken* — 1 row).

Hot-path tidy applied: `release-session-locks!` / `release-advisory-locks!`
now skip the global-atom rebuild when the session holds no matching lock
(they run per implicit-tx commit via `end-tx!`).

**Retire status:** the Clojure-side batchable path (`exec-batchable-insert`
+ its `exec-insert` branch) is removed — wire writes always take the
implicit-tx branch. The wire-layer scaffolding (`ExtBatchBuffer` held
bytes, `BatchBuffer`, `beginBatchScope`, `flushBatch`, `withBatchable`,
`discardBatch`) is now INERT (nothing returns `withBatchable`, so held
buffers stay empty and the flushes are no-ops). Removing it is a separate
wire-layer refactor with no functional/perf benefit; left in place +
documented to avoid risk.

## Performance watch-list (benchmark after)

- **+1 `dc/with` per single autocommit write.** A lone `INSERT` now goes
  speculative-then-commit instead of a direct `d/transact`. `dc/with` is
  in-memory (no konserve IO) so it should be cheap relative to the
  transact, but Odoo/Metabase do *many* single-row writes — re-measure.
- **Bulk INSERT (pg_dump / Odoo / Metabase init)** must stay
  commit-once-at-boundary (N × `dc/with` + 1 `d/transact`), matching the
  retired batchable path. Guard against accidental per-statement commit.
- These init paths were exercised by our integration tests/benchmarks;
  re-run those after Stage 2.
