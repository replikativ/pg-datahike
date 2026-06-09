# Correlated subqueries & LATERAL — design / scope (Option B)

Per-outer-row evaluation executor. Unblocks correlated scalar subqueries
(needed for asyncpg composite introspection) and, on the same seam, LATERAL
joins + correlated set-returning functions (Tier-A capability, task #14).

## Capabilities (target)
1. Correlated scalar subqueries in the SELECT list — **slice A** (unblocks
   composite introspection: `(SELECT array_agg(ia.attname ORDER BY ia.attnum)
   … WHERE c.reltype = t.oid)`).
2. Correlated scalar subqueries in WHERE.
3. LATERAL subquery joins (`FROM t, LATERAL (SELECT … =t.id) s`) — JSqlParser
   right-item is `LateralSubSelect`.
4. LATERAL set-returning functions (`FROM t, LATERAL generate_series(1,t.n)`)
   — right-item is `TableFunction` with correlated args.
(EXISTS/IN correlated subqueries in WHERE already work via or-join.)

## One mechanism: the per-outer-row seam (already exists)
Bind `*from-bindings* = {outer-alias → {col → val}}` (+ `*eval-update-db*`) for
one outer row, then evaluate the inner. Confirmed:
- `Column` translation resolves `alias.col` from `*from-bindings*`
  (expr.clj:1808).
- The UPDATE path already uses this for scalar subqueries in SET.
- `materialize-table-function` takes an `eval-fn`; `srf-const-eval` returns
  `::corr` for correlated args — the designed SRF seam.
So B = wiring this seam into the SELECT path, not new engine machinery.

## Two phases
- Translate (`stmt/translate-select`): detect correlation/LATERAL; tag parsed
  with the deferred inner(s) + the correlation vars (`alias.col` the inner
  references); add those vars to the outer query `:find` (hidden, stripped
  after projection).
- Execute (`server/exec-select`): run the outer query; per outer row bind
  `*from-bindings*` from the correlation vars, run the inner; splice:
  scalar→fill projection slot (0 rows→NULL); LATERAL join→cross-join
  (LEFT→null-extend); SRF→`materialize-table-function` + per-row `eval-fn`.

## Components
- C1 correlation detection + outer-var extraction (translate).
- C2 outer-var threading into outer `:find`.
- C3 per-row executor in exec-select (scalar / rows / SRF runners).
- C4 result splice; outer ORDER BY/LIMIT/DISTINCT applied after.

## Reused vs new
Reused: `*from-bindings*` (expr.clj:1808), `*eval-update-db*`,
`materialize-table-function` eval-fn seam, `translate-select`,
`array_agg`/`filter-array-agg`, correlated-EXISTS.
New: C1/C2 detection+threading, C3/C4 exec+splice, `LateralSubSelect`/
correlated-`TableFunction` detection in the FROM/JOIN loop, correlated SRF
`eval-fn`.

## Semantics
Scalar subquery: 0 rows→NULL; >1 row→PG errors (we can take-first leniently).
LATERAL LEFT JOIN: 0 inner rows→null-extend. Inner ORDER BY (array_agg ORDER
BY) handled by the inner query already. Nesting: `*from-bindings*` merges
(UPDATE path merges outer binding into existing). Multiple correlated
subqueries per SELECT: eval each per row.

## Performance
N+1 inner executions per outer row — fine for catalog/introspection (small N).
Future optimization (NOT the general path): decorrelate specific hot shapes
into one grouped Datalog query. That is the only place datahike's aggregates
matter, and it needs an ordered `array_agg`/`vec` aggregate (one-liner to add)
plus LEFT-join + ordering + decorrelation handling (hard). Deferred.
pg-datahike already implements `array_agg` (filter-array-agg) for the inner
queries, so no datahike change is needed for B.

## Risks
Correlation-detection completeness; threading hidden vars without breaking
projection/DISTINCT/aggregates; splice vs existing exec-select post-ops
(ORDER BY/LIMIT/window/aggregate); LATERAL `TableFunction` detection.

## Delivery plan (each slice reuses the C3 seam)
- A. correlated scalar subqueries in SELECT list (C1/C2/C3-scalar/C4-scalar) —
  unblocks composites. ← first.
- 2. correlated scalar subqueries in WHERE.
- 3. LATERAL subquery joins (`LateralSubSelect`) → C3-rows/C4-cross-join.
- 4. LATERAL SRFs (correlated `TableFunction` via eval-fn seam).
