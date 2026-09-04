# PostgreSQL design alignment

Where pg-datahike's architecture aligns with — and where it intentionally
diverges from — real PostgreSQL (compared against the `../postgres` checkout).
The goal is **a faithful PostgreSQL interface for realistic use** — covering the
feature behind what real clients (drivers, ORMs, migration tools, analysts) do —
not passing every adversarial conformance string.

This document describes the *design state*: the execution model, what that model
covers, the capabilities that fall outside it, and the boundaries that are
intrinsic to Datahike.

## The execution model

pg-datahike compiles each SQL statement into a **single Datalog query**
(`:find` / `:where` / `:in` / `:with` / `:order-by`) over **one immutable
Datahike snapshot**, plus light post-processing (HAVING, window functions,
ORDER BY, `FOR UPDATE` locks). PostgreSQL is a **tuple-at-a-time iterator
(volcano) executor with arbitrary nesting over MVCC storage**.

That single-query model is elegant and covers the bulk of CRUD and analytics.
Within it:

- Joins compile to one flat `:where` with shared logic vars; OUTER joins via
  `or-join`; correlated `EXISTS` via `not-join`.
- Derived tables / table functions are materialized once into a virtual table
  (`d/db-with`).
- Multi-statement scripts run under an implicit-transaction model so a later
  statement sees an earlier one's writes.

A few capabilities live *outside* the single-query model (structural gaps
below); most remaining differences are incremental type/catalog coverage, or
boundaries intrinsic to Datahike.

## Secondary access paths

Secondary indexes are immutable generations named from the same Datahike root
as the primary indexes. Preparing a transaction may write content-addressed
secondary objects, but the Datahike root is the sole publication point. A
branch or historical database value consequently resolves the corresponding
secondary generation rather than a mutable external `latest` pointer.

The query boundary describes candidate precision, recall, and ordering
independently. This covers three materially different PostgreSQL shapes:

- GIN/GiST-style full-text candidates can be complete but require an exact
  PostgreSQL `@@` recheck;
- pgvector HNSW candidates have approximate recall even though returned
  distances are rechecked and sorted exactly;
- B-tree-shaped Stratum pages have complete recall and exact value ordering.

External scans can declare that they accept an upstream entity bitmap or that
one is required. The latter is a planner dependency, not merely an adapter
hint: primary predicates run first, their current relation is projected to an
`EntityBitSet`, and filtered or ordered retrieval receives it. Missing required
input fails closed. This supports pre-filtered ANN and cross-index bitmap
composition without materializing entity IDs in the PostgreSQL adapter.

Unavailable, stale, or incompatible secondary generations are optional access
paths and fall back to the primary query. PostgreSQL predicates and distance
functions remain authoritative. The SQL vertical is experimental while its
Datahike, Scriptum, Proximum, and Stratum changes are reviewed together. The
maintainer test matrix and performance gates live in the
[secondary-index validation guide](../test/integration/postgres-regress/SECONDARY_INDEXES.md).

Dropping a secondary declaration removes its generation address from the new
Datahike root without mutating retained roots. Cascading that operation from an
empty SQL table is covered. Dropping and recreating populated relations under
history retention still needs generation-scoped schema identities; weakening
Datahike's schema-transition guard would make historical datoms adopt the new
relation's schema and is not an acceptable shortcut.

The current protocol is broad enough for tuple-producing B-tree, inverted
full-text, and KNN/ANN implementations. It is not yet a complete analogue of
PostgreSQL's access-method registry: operator classes are mapped by the SQL
adapter, and expression/partial/multicolumn/INCLUDE definitions lack a
normalized planner descriptor. BRIN also exposes a performance-level gap:
candidate pages enumerate entity IDs, while a summarizing index needs to hand
the primary engine compact ranges or partitions to scan. Those are explicit
next interfaces rather than reasons to weaken the immutable-generation model.

All current adapters use Konserve and therefore share its values-before-root
GC fence. Storage ownership still matters. Datahike directly marks Scriptum
and Stratum objects in its store; Proximum's separate store needs a complete
export of generation IDs retained by Datahike history before its own collector
can safely sweep. A shared fencing mechanism solves the publication race, not
cross-store root discovery.

## Covered

Capabilities implemented and exercised by the conformance suites:

- **DML + analytics**: SELECT/INSERT/UPDATE/DELETE, joins (inner/outer/anti),
  subqueries, CTEs (incl. recursive), aggregates, window functions, DISTINCT,
  ORDER BY, LIMIT/OFFSET, `FOR UPDATE`.
- **Multi-statement visibility**: `CREATE; INSERT; SELECT` in one string — the
  SELECT sees the INSERT (implicit-transaction model across the batch).
- **Set-returning functions in FROM with constant args** (`generate_series`,
  `unnest`) materialised into the virtual-table path.
- **Composite / record types**: `CREATE TYPE … AS (…)`, `ROW(...)`, named and
  anonymous records, text and binary codecs, composite parameters.
- **Type fidelity**: int2/int4/int8/oid widths, float/numeric, bool, uuid,
  text/varchar/bpchar, bytea (PG hex), json/jsonb, date/time/timestamp(tz),
  arrays (incl. multi-dimensional and non-default bounds) — including correct
  result OIDs for `::T` and `::T[]` casts.
- **Catalog introspection** sufficient for driver/codec setup: `pg_type`,
  `pg_class`, `pg_attribute`, `pg_namespace`, and the recursive type-info
  queries asyncpg/JDBC issue to build codecs.
- **Transactions**: BEGIN/COMMIT/ROLLBACK, savepoints, isolation-level
  reporting; extended-protocol Parse/Bind/Describe/Execute, executemany.

## Structural gaps (outside the single-query model)

These need execution beyond one `d/q` over one snapshot:

- **OUTER LATERAL** (`LEFT JOIN LATERAL (…) ON true`). An outer lateral keeps
  the outer row with NULLs when the inner is empty; an empty collection binding
  DROPS it, which is precisely the inner-join semantics the lateral support
  relies on. Reproducing the outer form needs the `or-join` construction the
  other OUTER joins use. Refused explicitly (0A000) rather than answered wrong.

  **LATERAL itself is no longer a structural gap** — that was a mistaken
  reading of the engine. Datahike's `bind-by-fn` applies a function once per
  production tuple and expands the result through the binding form, so
  `[(f ?n) [[?v ?ord]]]` already evaluates per outer row inside the ordinary
  flat `:where`. Both `FROM t, LATERAL generate_series(1, t.n)` and
  `JOIN LATERAL (SELECT … WHERE x = t.id) s ON true` compile to ONE Datalog
  query with no speculative data — the subquery case by running the inner
  through that same binding, with `*from-bindings*` holding the outer values.
  That is why they keep the parse cache and fast-select lanes a
  materialisation approach would forfeit.
- **Query-result streaming**. Extended-protocol `Execute` honors a row cap,
  emits `PortalSuspended`, and resumes the same portal without rerunning the
  statement, so driver `fetchSize` and asyncpg iterable cursors are
  protocol-correct. Datahike queries are not generally lazy, so pg-datahike
  also enforces a deployment ceiling of 100,000 result rows by default and
  fails larger results with SQLSTATE `54000` before allocating the pgwire
  `String[][]`. Set-shaped SELECTs push ceiling+1 into `d/q`; shapes that
  must first reduce or expand rows are checked after their semantic pipeline.
  This bounds returned result materialisation, not every query's intermediate
  working set or first-row latency.
- **ProjectSet around grouping, windows, and `DISTINCT ON`**. Target-list SRFs
  now expand into rows, zip same-level SRFs with NULL padding, preserve nested
  levels, and feed derived tables, CTAS, INSERT…SELECT, set operations, sorting,
  and limits. PostgreSQL can place different ProjectSet levels on either side
  of grouping, window, and distinct stages according to their references; the
  combinations that need that movable stage are still rejected explicitly.

## Coverage tail (incremental, fits the model)

- **Range / multirange types** (`int4range`, `tsrange`, …): text round-trips,
  but the binary Range codec and range constructor functions are not yet
  implemented.
- **Remaining type tail**: structural `interval`, the internal `"char"`,
  `aclitem`, range types, and less common polymorphic/operator overloads.
- **Wider catalog coverage**: `pg_stat_activity`, `pg_index`, `pg_constraint`,
  `pg_proc`, etc. — added as tools require them.
- **Extensions / `CREATE FUNCTION` / plpgsql / `DO`**: not implemented.

## Intrinsic boundaries (Datahike)

These are inherent to the storage model and documented as boundaries, not gaps
to close:

- **Concurrent multi-writer MVCC isolation**: Datahike is single-writer over
  immutable snapshots. `FOR UPDATE` / advisory locks / savepoints are
  approximated; genuinely concurrent isolation levels are not the model. This is
  the most important boundary for pooled workloads.
- **Per-session `pg_temp` isolation**: drop-on-disconnect covers sequential use;
  concurrent same-name temp tables across sessions are not namespaced.
- **Adversarial parser edge cases** (e.g. a backslash inside a quoted
  identifier): a jsqlparser lexer limitation.
