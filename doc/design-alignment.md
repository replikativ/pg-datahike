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
- **Bounded query-result streaming**. Extended-protocol `Execute` now honors a
  row cap, emits `PortalSuspended`, and resumes the same portal without
  rerunning the statement, so driver `fetchSize` and asyncpg iterable cursors
  are protocol-correct. The backing `d/q` relation and its `String[][]` wire
  representation are still materialised eagerly, however; portal paging does
  not yet bound server heap or improve first-row latency for large results.
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
