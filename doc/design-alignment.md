# PostgreSQL design alignment & deep-feature roadmap

Status: living document. Captures where pg-datahike's architecture diverges
from real PostgreSQL (compared against the `../postgres` checkout, 19devel /
REL_18 base), why, and the planned sequence for closing realistic-use gaps.

The agenda is **a solid PostgreSQL interface for realistic use cases** — not
passing every adversarial conformance string, but covering the *feature*
behind each thing real clients (drivers, ORMs, migration tools, analysts)
actually do.

## The one root mismatch

pg-datahike compiles each SQL statement into a **single Datalog query**
(`:find` / `:where` / `:in` / `:with` / `:order-by`) over **one immutable
Datahike snapshot**, plus light post-processing (HAVING, window functions,
ORDER BY, FOR UPDATE locks). PostgreSQL is a **tuple-at-a-time iterator
(volcano) executor with arbitrary nesting, over MVCC storage**.

That single-query model is elegant and covers the bulk of CRUD + analytics,
but four realistic capabilities live *outside* it (Tier A below). Most other
gaps are incremental coverage that fits the model unchanged (Tier B), or are
intrinsic to Datahike and best documented as boundaries (Tier C).

Joins today compile to one flat `:where` with shared logic vars; OUTER joins
via `or-join`; correlated `EXISTS` via `not-join`. Derived tables / table
functions are **materialized once at parse time** into a virtual table
(`d/db-with`) — so they cannot see an outer row.

## Tier A — execution-model gaps (structural, high realistic value)

### A1. Multi-statement visibility
`CREATE; INSERT; SELECT` in one simple-query string: the SELECT must see the
INSERT. Migrations / psql scripts / app init rely on this. Needs each
statement in a batch to commit (or accumulate into a speculative db) before
the next reads. **Investigate the current batch model first** — may be a
small fix if statements already run sequentially on the conn.

### A2. Set-returning functions (SRF)
- **In FROM, constant args** (`generate_series(2,4)`, `FROM now()`):
  CHEAP. Infrastructure exists — `materialize-table-function`
  (`stmt.clj`) already does `unnest(ARRAY[…])` and feeds the virtual-table
  path. Extend it + add a bare-`TableFunction` branch to the FROM dispatch.
  Fits the single-query model. **Build as a LATERAL-ready seam** (see below).
- **In FROM, correlated** (`LATERAL generate_series(1, t.n)`): needs A4.
- **In the SELECT list** (`SELECT generate_series(1,3)` → N rows; PG's
  ProjectSet node): we currently serialize SRFs in SELECT instead of
  expanding to rows. Less common; defer.

### A3. Portal streaming / row-limits
Extended-protocol `Execute` with a row cap → `PortalSuspended` (JDBC
`fetchSize`, node-postgres `rows:`, server-side cursors over large results).
We materialize all rows in one `d/q`. Matters for large results + memory.
Moderate effort (we already do eager `DECLARE` cursors; extend to Bind
portals).

### A4. LATERAL joins  →  the nested-loop executor
PG (`optimizer/README:1209`) implements LATERAL as a **parameterized
nested-loop**: the inner FROM item is parameterized by the relations it
references and must sit on the inside of a nestloop; i.e. **for each outer
row, evaluate the inner subquery with the outer columns bound, then
concatenate**. (Sometimes flattened to a plain join, but the nestloop is the
fallback.)

The nested-loop **primitive already exists**, just on the UPDATE path:
- `build-update-tx-for-bindings` (`server.clj:1929`) iterates matched eids,
  per-row binds the outer row into `*from-bindings*`, and runs inner scalar
  subqueries (`*eval-update-db*`, scalar-subquery branch `stmt.clj:3114`).
- `UPDATE … FROM (VALUES …)` reduces over rows (`server.clj:2034`).
- `expr.clj`'s Column branch resolves `alias.col` against `*from-bindings*`.

LATERAL = porting that per-row nested-loop to the SELECT path:
1. Front-end: a `Join` whose right item is a `LateralSubSelect` (JSqlParser
   surfaces this), plus the `FROM a, LATERAL (…)` comma form.
2. Plan split: outer = relations left of the lateral; inner = the lateral
   subquery / correlated table-function.
3. Execute (new SELECT nested-loop branch): run outer → per outer row, bind
   its columns into `*from-bindings*`, run the inner per-row (`d/q`), emit
   `outer ⧺ inner` per inner row; `LEFT JOIN LATERAL … ON TRUE` null-extends
   when the inner is empty.
4. Outer projection / WHERE / ORDER / LIMIT run over the combined row stream,
   not a single `d/q`.

First cut: `CROSS/LEFT JOIN LATERAL (subquery|SRF) ON TRUE` + comma form,
nested-loop only (document O(outer×inner) cost), no aggregation across the
lateral boundary. Multi-day; the hard primitive is built and proven.

## Tier B — semantics / coverage gaps (incremental, no model change)

- **Error on unknown relation (`42P01`)**: EAV returns empty for a missing
  table; real apps want the error (catches typos / missing migrations). Plan:
  raise when a FROM namespace has zero attrs and isn't a view/CTE/function,
  keeping column-level EAV permissiveness. Design call — leaning yes.
- **Type fidelity tail**: int2/int4 done (`:pg/type` hints). Remaining:
  `oid`(26), `real`/float4(700) vs double, `numeric` precision round-trip,
  `timetz`/`interval`. Each is a `:pg/type` hint + coercion. Also: no integer
  range enforcement (`int4` column accepts > 2³¹; PG errors).
- **Catalog coverage**: `pg_class`/`pg_attribute` partial; tools also query
  `pg_stat_activity`, `pg_index`, `pg_constraint`, `pg_proc`. Add on demand.
- **Cancellation SQL surface**: `pg_cancel_backend()` + `pg_stat_activity`
  view. The common cancellation path (driver `.cancel()`, Ctrl-C,
  `statement_timeout`) already works via the wire `CancelRequest` infra;
  this is the admin/tooling surface.
- **Extended multi-statement → `42601`**: reject a multi-statement string in
  the extended (Parse) path. Small; needs reliable multi-statement detection
  so it doesn't break the working simple-query multi-statement path.

## Tier C — accept & document (intrinsic to Datahike)

- **True concurrent MVCC across connections**: Datahike is single-writer with
  immutable snapshots; we approximate `FOR UPDATE` / advisory locks /
  savepoints. Genuinely concurrent multi-writer isolation levels are not the
  Datahike model. The boundary to be most explicit about for pooled workloads.
- **Full per-session `pg_temp` isolation**: drop-on-disconnect covers
  sequential use; concurrent same-name temp tables would need per-session
  namespacing of every table reference. Rare.
- **Adversarial parser edge cases** (e.g. backslash inside a quoted
  identifier): jsqlparser lexer limitation, low real-world value.

## Recommended sequence (and why it avoids rework)

The concern "do LATERAL first so we don't refactor after" is reasonable, but
the smaller pieces are either **reused by** LATERAL or **orthogonal** to it —
none get refactored away — *provided* we build the SRF materialization with a
clean seam:

> **Seam:** `materialize-table-function` takes **already-evaluated argument
> values**, not raw literal AST. The constant path evaluates literal args
> once; the LATERAL path evaluates correlated args (`t.n`) per outer row from
> `*from-bindings*`. Same helper, both callers. Designing it this way now =
> zero rework when LATERAL lands.

Suggested order:
1. **A2 constant-arg SRF-in-FROM** — cheap, high value, becomes a LATERAL
   building block (built with the seam above).
2. **A1 multi-statement visibility** — orthogonal to query shape; high value
   for scripts/migrations. Investigate batch model first.
3. **B unknown-relation `42601`/`42P01` + type tail** — orthogonal,
   incremental correctness wins.
4. **A4 LATERAL** — the nested-loop executor; reuses #1's materialization for
   correlated SRFs and the UPDATE path's per-row machinery. The big
   investment, done once the cheap wins are banked and the design is settled.
5. **A3 portal streaming** — orthogonal result-delivery concern.

LATERAL is **not** a prerequisite for 1–3, and 1–3 are not throwaway work for
4. The only shared touch-point is the FROM/JOIN dispatch in `translate-select`,
which gains an additive `LateralSubSelect` branch — not a rewrite.
