# Atomic constraint admission

## Decision under validation

Use one ordinary Datahike transaction to migrate the PostgreSQL catalog and
validate the resulting candidate before exposing a SQL handler. A separate
writer barrier is unnecessary for this workflow. Predicate registration stays
outside the transaction because it changes a process-local registry and cannot
be rolled back with database state.

The transaction contains two sequential transaction-function calls:

1. Compute and apply schema/catalog migration from the writer's input database.
2. Validate all durable unique-index descriptors and their existing rows, then
   return no further transaction data.

Nested migration functions run before the second call. The normal writer
predicate also checks the final report. A rejected admission publishes neither
the migration nor readiness. Even an already migrated branch gets one real
admission transaction; subsequent cache hits on that writer do not write.

## Ordering argument

A pre-registration report may already exist in the writer's speculative chain.
The admission transaction sees its effects and rejects invalid data. If a head
conflict instead causes that old invocation to replay, it runs through the
newly installed predicate. The admission transaction itself must also rerun its
validation when replayed, rather than carry a precomputed validation result.

This does not require every earlier invocation to finish before admission can
succeed. An invocation still awaiting replay cannot publish its rejected old
report; its replay must encounter the guard. This is a constraint invariant,
not the stronger queue-draining contract of a general writer barrier.

The guarantee assumes writers use Datahike's governed write path. Registration
is process-local: a different process without the guard remains ungoverned.
Remote writer admission continues to fail explicitly until guard deployment
on that writer is supported.

## Evidence — 2026-09-06

- Unmodified Datahike main `cac671a5`: controlled pre-registration in-flight
  writes, exclusive/shared ownership, and shared conflict replay passed 13
  assertions in `pg_admission_transaction_test.clj`.
- Atomic pg-datahike admission against a narrowed dependency with no barrier
  API: 35 unique-index/admission tests, 198 assertions, zero failures/errors.
  These include concurrent admission, no writes on cache hits, and rollback
  of legacy catalog migration when admission validation fails. The separately
  tracked native NaN regression was excluded from this focused run, not removed.
- Unmodified main accepted `:db.unique/value` on an already indexed string
  attribute holding duplicate values with `:allow-index-backfill? true`.
  Therefore existing connection-level configuration is not by itself a safe
  replacement for all of the proposed backfill changes.

Full candidate matrices remain required before publishing the replacement PR.

## Smaller Datahike dependency

Retain named, atomic predicate registration, transaction-local backfill
options, and duplicate validation when an indexed attribute becomes unique.
Main has only one overwriteable predicate slot per store; a pg-datahike-only
lock would not safely coordinate independent consumers of that slot.

Remove the proposed barrier API, queue/retry changes, and Kabel readiness
changes from the dependency needed by pg-datahike. Avoid temporarily mutating
connection configuration to enable backfill: that would expose the policy to
concurrent writes and require reliable restoration after failures.
