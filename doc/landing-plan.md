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

## datahike (worktree branch feature/index-backfill, 10 commits)

Split for upstream as independent PRs, in this order:

1. **Mechanical, no semantics**: 8da03459 bucket-weight O(1); e240d60a
   query-cache bucket cap. Trivial review.
2. **Caching, semantics-neutral**: 9a22574e form-analysis memoization +
   `*result-cache-min-weight*`; b0c8ad78 critic fixes fold into this PR
   (BigDecimal-scale key canonicalization is part of the memoization
   story).
3. **Prepared execution** (the big one): 71074449 + its half of b0c8ad78.
   Needs a design note: value-free plan keys, single-tuple-rel absorption,
   compiled point programs, lookup-ref routing rule. Ship with the
   14-shape × 4-engine differential as a test.
4. **Behavioral features, each with an issue first**: e1fb24bb
   index-backfill migration; 9b53c281 `:sync-commit?` writer mode;
   b7ddda93 drop-attr check on db-after.
5. **Upstream issue (no code)**: repeated get-else output vars diverge
   between legacy (rebind) and planner engine (equality) — pre-existing,
   found by the critic pass.

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
