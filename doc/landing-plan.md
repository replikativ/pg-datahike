# Landing plan — performance campaign (2026-08)

Goal: keep the perf push aggressive while every piece stays individually
landable. Rules: no slice is committed without its gate green; anything
headed upstream gets an adversarial critic pass (`codex exec -m gpt-5.6-sol`,
read-only sandbox) first; benchmarks recorded in the memory ledger with
load-average noted.

## pg-datahike (main, already landed)

Committed on main through b4bbd05, each gate-green:
- 5a0ce32 result-cache put threshold binding
- aebb3b5 execute-path caches (shape / FK metadata / row-match translation)
- b4bbd05 wire: platform conn threads, Sync-only flush, tier-1 compiled
  SELECT lane

Follow-ups that refactor rather than add:
- Promote the tier-1 lane + the aebb3b5 caches into a single
  `CompiledStatement` record built at Parse (tiers: point program /
  generic direct / relation fallback). One dispatch point; delete the
  scattered caches as they get absorbed.
- Optional: fork jsqlparser once the CompiledStatement shape is stable
  (tier-0: parser emits our IR directly; removes the ad-hoc expansion
  layer).

## datahike (worktree branch feature/index-backfill, ~860 insertions)

Landing-risk review (critic pass 3, gpt-5.6-sol) ranked the risks and
proposed the split below. Three of its findings were fixed immediately
(commit pending): normalize-q-input memoized on the WHOLE query input,
retaining DB snapshots in the LRU for map-form calls (now keys on the
query form only); the async writer re-read a possibly-stale parent
commit-id from the connection between commits (now threads the returned
id); index-backfill used java.util.HashSet in cljc (CLJS blocker) with
JVM equality (misses array value-equality — now a set of
arr/wrap-comparable keys).

PR order (each gated + critic-passed, rebased onto post-merge main):

1. 8da03459 bucket-weight O(1) — pure win, zero semantic surface.
2. e240d60a cache-propagation cap (make threshold configurable).
3. b7ddda93 drop-attr check on db-after (narrow, semantic — alone).
4. Inert tuning seams: 71d1083f `*fold-scalar-ins*` + the
   `*result-cache-min-weight*` half of 9a22574e (defaults unchanged).
5. ~~`:sync-commit?` writer~~ — WITHDRAWN (#933 closed): a controlled
   A/B on the final stack showed the writer's pipelining already
   swallows the memory-store commit cost (sync mode reproduces the
   PG-parity numbers: tpcb c1 95 / c4 303 / c8 361). The flag only
   reorders ack vs an in-RAM step on :memory and trades real
   durability on durable backends. The genuine writer opportunity is
   group-commit batching (+50-80% in isolated experiments, ~35k tps
   direct-PSS ceiling) — a separate design-first proposal. The stack
   no longer touches writer.cljc at all.
6. ~~index-backfill~~ — LANDED (#934, squash-merged) behind
   `:allow-index-backfill?` (default false). FOLLOW-UP design note
   (from whilo): secondary indices backfill ASYNC in the background —
   the same pattern could serve `:db/index` enablement (pure index
   population, no acceptance semantics). `:db/unique` addition must
   stay synchronous: the transaction's acceptance depends on the
   duplicate check completing atomically with the schema change.
   Revisit splitting the two paths after the campaign lands.
7. Form-analysis memoization (rest of 9a22574e + BigDecimal fix) —
   consider default-off flag; fix double-LRU-entry capacity cost.
8. **Prepared execution** (71074449 + critic fixes) — behind a dynamic
   var defaulting OFF so it lands inert (pg-datahike binds it on). The
   unconditionally-active pieces the critic flagged must fold under the
   flag or be justified separately: try-point-group inside
   execute-plan-direct (affects fold-on users), 64-slot result-list,
   empty-consts lookup shortcut, plan-map :program-cache atom (breaks
   plan value-equality/printing — consider metadata instead of a map
   key). Ship with the 14-shape × 4-engine differential as a test plus
   a generative differential.
9. Upstream issue (no code): repeated get-else output vars diverge
   between legacy (rebind) and planner engine (equality) — pre-existing.

## Open perf fronts (in priority order)

- tpcb c4 contention: replace optimistic commit aborts on hot rows with
  blocking row locks at UPDATE time (PG-faithful; lock infra exists).
- Writer ack ~1.2 ms per commit (datahike writer #16); group-commit in
  the writer (experiment: +50-80%, scales).
- Extend CompiledStatement tiers to UPDATE/INSERT.
- Aggregate streaming eligibility (62× gap on whole-table sum/count).

## Write-concurrency model (2026-08-01 slice) — known deltas vs PostgreSQL

Implemented: blocking row locks for in-tx UPDATE/DELETE + snapshot rebase
(READ COMMITTED per-statement anchoring), never-scan ring conflict check
(grace-retry then conservative 40001), fresh-insert write-set attribution,
per-database ring keying, commit check+transact under a global monitor,
savepoints snapshot the conflict watermark. Adversarially reviewed
(critic pass 2); accepted deltas, documented not fixed:

- Rebase-and-retry after a lock wait re-runs the WHOLE statement predicate
  against the new snapshot (PG re-checks only the blocked tuple —
  EvalPlanQual). Rows that newly match during the wait can be affected.
- Deadlock cycles across statements resolve by lock-wait timeout (2s →
  40001), not a cycle detector; polling acquisition has no FIFO fairness.
- Autocommit (non-tx) writes don't take row locks; they rely on the
  commit-time conflict check only.
- INSERT…SELECT / schema-map inserts are attributed conservatively
  (::opaque when a map key is a unique attr) → they abort concurrent
  windows rather than risk missed upsert conflicts.

## Current standings (clean, scale 8, vs PostgreSQL 17 disk)

select c1 2754 vs 5732 (2.1×) · select c8 26320 vs ~40k (~1.5×) ·
tpcb c1 133 vs 251 (1.9×) · **tpcb c4 302 vs 302 (parity, 0 failures)** ·
tpcb c8 266-339.
