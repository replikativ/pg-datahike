# Background backfill follow-up

Planning review after Datahike #1077, merged as `bbbf4569`. This is a proposed
implementation sequence, not a claim that background AVET builds exist today.
The synchronous transaction-local option remains useful for small migrations.

## Required contract

Build from an exact pinned snapshot while writes continue. Readers retain the
old complete index and effective schema until atomic activation of the new
roots and schema. Requested indexing/uniqueness lives in a separate build
descriptor while construction is underway. In particular, identity upsert
and lookup-ref semantics must not activate against an incomplete AVET.

pg-datahike admission and ordinary CREATE UNIQUE INDEX must await successful
activation. Accepting a build request is not successful schema migration.
New uniqueness can fail if concurrent writes introduce duplicates; existing
constraints remain enforced throughout construction.

Bound memory in bytes, not just datom counts. Include sorting, merge fan-in,
open files, pending node writes, concurrent-write capture, and final catch-up.
Specify oversized-value handling and temporary-disk/lag quotas. A slow worker
must cause explicit backpressure or build failure, never silent delta loss.

## Existing code to reuse and audit

- `datahike.db.transaction`: secondary declaration/readiness and live-update
  capture. The current `:secondary-index-build-deltas` vector is unbounded.
- `datahike.writing`: snapshot scan, generation checks, serialized installation,
  bulk node flushing, and local-exclusive-writer restriction.
- `datahike.connector`: restart by reanchoring at a durable head and rebuilding.
- `datahike.migrate.init` and `datahike.migrate.sort`: external sorting and
  sorted persistent-set construction. Reuse these primitives, not the entire
  import workflow. Audit record-based run limits and growing run-file lists
  before claiming dataset-independent memory bounds.
- `datahike.gc-roots`: source pins and partial-tree checkpoints. Unpublished
  nodes and journal segments need protection through durable publication.

## Proposed sequence

1. Replace the secondary build's in-memory journal with bounded immutable
   segments and a small generation/cursor descriptor. Capture committed
   effects in commit preparation; pure `d/with` performs no journal I/O.
   Speculative or aborted reports cannot advance committed cursors. Old
   database snapshots must not retain an ever-growing journal vector.
2. Establish byte-based sort/build budgets, bounded merge fan-in and journal
   catch-up. Test throttling or quota failure with a deliberately paused worker.
3. Add AVET construction and atomic activation. AVET is one tree shared by
   attributes: a private attribute tree cannot simply replace the live root.
   Compare a complete AVET-family rebuild (simpler, more I/O) against a bounded
   persistent-set range-splice capability before selecting the implementation.
   Either choice must preserve unrelated indexed attributes and their writes.
4. Validate new uniqueness on the caught-up private index outside the writer;
   subsequent replay maintains validity. Bound the final serialized tail rather
   than scanning the whole attribute again while blocking the writer.
5. Exercise restart, GC, fencing, cancellation, and stale-build rejection.
   Initially restart interrupted builds from a fresh durable snapshot; durable
   journal storage alone does not establish resumability.

Initial proposed online-AVET scope: JVM, persistent-set, local exclusive writer,
and durable scratch storage. Keep coordination data portable. Node needs worker
execution or cooperative I/O, not synchronous sorting on its event loop. Refuse
unsupported browser/HHT online builds explicitly. Both attribute-reference
representations should work; no ID remapping is needed.

## Release gates

- A dataset larger than the fixed test heap, with concurrent inserts, updates,
  retractions, retained old snapshots, history, and unrelated indexed attributes.
- Duplicate introduction/repair during scan, replay and final activation;
  byte arrays and tuples; identity behavior before and after activation.
- Exact current/history AVET comparison with the synchronous migration oracle.
- Crash and GC races around segment persistence, checkpoints and activation;
  cancellation/recreation, branch movement, expired leases and writer takeover.
- Measured buffer, scratch and final-tail bounds under sustained write pressure.
  Adapter-specific secondary build memory remains a separate capability claim.

Use a fresh build ID and branch/owner identity, not max-tx alone. Publication
must check generation, schema prerequisites, caught-up cursor/head, fencing,
and live GC protection. Relevant schema mutations should invalidate/restart a
build until their replay semantics are explicitly supported.
