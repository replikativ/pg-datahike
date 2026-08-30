# Benchmark workload map

This is a maintainer-facing complexity audit for the benchmark suite. Throughput
ratios are useful only after the two engines have comparable access paths. For
each statement we therefore record the physical path, the amount of data
touched, whether `LIMIT` bounds upstream work, and where PostgreSQL semantics
require a primary-value recheck.

Notation:

- `N`: rows in the table
- `s`: rows admitted by a filter
- `k`: `LIMIT + OFFSET`
- `d`: vector dimension
- `p`: full-text posting-list work
- `m`: projected columns

## Measurement rules

1. Compare growth at several `N`, not one latency ratio.
2. Record PostgreSQL with `EXPLAIN (ANALYZE, BUFFERS)` and capture the actual
   Datalog calls made by pg-datahike.
3. Count candidates at every boundary: primary prefilter, secondary search,
   authoritative recheck, and emitted rows.
4. Treat Datahike `:stats?` results carefully. At present statistics select the
   legacy relational executor and therefore do not describe the optimized
   direct/planned path. Use `datahike.query/explain` plus call-site/candidate
   instrumentation for the production path.
5. A constant-factor gap is acceptable only when both paths have the same
   scaling and bounded-memory properties.

## pgbench

| Statement | PostgreSQL path | pg-datahike/Datahike path | Complexity assessment |
|---|---|---|---|
| `SELECT abalance ... WHERE aid = ?` | unique B-tree seek, one heap tuple | unique AVET seek, then one EAVT lookup for `abalance` | Same `O(log N)` lookup class. Datahike pays a second tree seek because a row is stored as attributes. |
| `UPDATE accounts SET abalance = abalance + ? WHERE aid = ?` | unique B-tree seek, heap/WAL/index maintenance | unique AVET seek, EAVT value read, immutable EAVT/AEVT/AVET transaction update | Same logarithmic selection/update class; persistent-tree object writes and root publication add write amplification. |
| teller/branch point updates | same as account update, but highly contended | same point-update shape, optimistic head conflict/retry | Same access complexity. Contention and retry rate, not scan complexity, dominate at concurrency. |
| history insert | heap/WAL append plus maintained indexes | new entity plus facts in the three primary indexes | Same `O(log N)` index-update class, with larger constants and storage amplification on the immutable EAV model. |

The select-only result therefore measures mostly constant factors: PG-wire and
SQL lowering, plan reuse, two B-tree-family seeks instead of an index seek plus
heap fetch, tuple construction, and JVM allocation. The TPC-B result additionally
measures immutable index writes, commit-root publication, and conflict retries.

## Secondary benchmark

| Workload | PostgreSQL path | pg-datahike path | Current complexity verdict |
|---|---|---|---|
| unindexed scalar `ORDER BY rank LIMIT k` | sequential scan plus bounded top-N heap, `O(N log k)` memory `O(k)` | generic Datahike result construction plus full sort, `O(N log N)` memory `O(N)` | **Structural gap.** Add bounded top-N to the generic ordered-result path. |
| indexed scalar order | backward B-tree scan stopped by `Limit`, `O(log N + k)` | exact AVET forward/reverse slice when SQL/Datahike ordering is proven compatible; otherwise exact Stratum page and primary recheck | AVET path has the right shape. Stratum one-page top-N is `O(N log k)` today, not B-tree-shaped; OFFSET continuations can repeat that work. |
| unindexed full text | sequential row scan and exact `@@` evaluation | primary tsvector scan and exact PostgreSQL-compatible matcher | Same linear class. Datahike performs attribute-oriented reads rather than one heap-row fetch. |
| GIN/Scriptum full-text filter | posting-list/bitmap combination, heap recheck, then requested SQL order | Lucene posting-list query to an entity bitmap, planner pushdown, authoritative tsvector recheck, then requested SQL order | Same `O(p + candidates)` access class. Entity-filter composition is pushed into Lucene. Ranking remains an exact primary operation. |
| exact vector top-k | sequential distance scan plus bounded top-N heap, `O(N*d + N log k)` | sequential distance evaluation followed by generic full sort in the fallback | **Structural gap in the baseline.** Distance work matches; result selection should be `O(N log k)`, not `O(N log N)`. |
| unfiltered HNSW top-k | pgvector HNSW search, then heap visibility/recheck | Proximum HNSW search, bounded entity candidates, then exact distance/projection recheck | Same graph-search class plus `O(k)` primary recheck. Recall/beam and deterministic tie ordering must be compared separately. |
| filtered HNSW, moderate selectivity | pgvector normally searches HNSW then filters heap rows; iterative scan resumes graph search until enough rows or budget exhaustion | bounded ANN probe and/or Proximum filtered HNSW using one primary entity bitmap, followed by authoritative recheck | Comparable graph-search class. pg-datahike avoids rerunning the SQL predicate on every ANN page. |
| filtered vector, sparse | pgvector iterative scan visits roughly `k/selectivity` ordered candidates; planner may choose a different primary plan when available | one AVET primary prefilter, Proximum exact top-k over selected IDs, final `k`-row recheck | Desired shape is `O(log N + s*d + s log k)`. Current Proximum filter translation/cardinality walk adds `O(N/word-size)` dense-bitset work and `O(s log N)` external-ID translation. |

## Evidence from the current 10k fixture

PostgreSQL 17.10 with pgvector 0.8.0 reports:

- scalar indexed top-10: backward B-tree scan emits 10 rows and touches three
  shared buffers;
- 10% full text: GIN emits 1,000 TIDs, the bitmap heap scan rechecks 1,000 rows,
  and SQL order sorts those 1,000 rows;
- unfiltered HNSW: the index emits 10 rows directly;
- filtered HNSW with iterative scan disabled: a 10% predicate emits 4/10 and a
  1% predicate emits 1/10 after filtering the first 40 ANN results;
- with `hnsw.iterative_scan = strict_order`, it rejects 89 rows to fill the 10%
  query and 971 rows to fill the 1% query.

The live pg-datahike REPL trace for `id < 10 ORDER BY embedding <=> ? LIMIT 10`
shows the intended sparse shape: an AVET-bounded primary prefilter produces ten
entities, Proximum receives that entity set, and the final Datalog query is
restricted to those candidates. It does not scan all stored vectors. The
ordinary Datahike statistics path misleadingly reports a 10k scan because
enabling statistics switches away from the optimized planner.

## Cross-cutting structural gaps

1. **Wide-row projection.** PostgreSQL obtains `m` columns from one heap tuple.
   Datahike normally performs one driving seek plus up to `m-1` EAVT seeks.
   Fused entity groups reduce allocation but not the number of tree lookups.
   This is `O(m log N)` versus `O(log N + m)` and needs explicit width scaling.
2. **Demand propagation.** PostgreSQL's pull executor lets `Limit` stop its
   child. Datahike propagates demand for a proven-safe single entity group, but
   post-filters, multiple groups, many joins, aggregation, and generic ordering
   still materialize upstream results.
3. **Sort and spill.** PostgreSQL has bounded top-N and external sort. Generic
   Datahike ordering fully sorts in memory. Hash joins and aggregates also lack
   PostgreSQL's general spill machinery, so equal CPU big-O can still become an
   OOM cliff.
4. **Stratum pagination.** Candidate continuations carry an offset. A later page
   can recompute and discard the prefix; keyset/cursor continuation is required
   before calling this B-tree-equivalent pagination.
5. **Sparse Proximum filters.** Exact distance evaluation walks only set bits,
   but allocating the dense bitset, computing its cardinality, and crossing
   zero words still scale with total vector capacity. A sorted internal-ID
   representation should keep the sparse lane proportional to `s`.

## Acceptance matrix

Run each workload at `N = 10k, 100k, 1m` where memory permits, with fixed `k`
and fixed selectivity. Record candidate counts and allocation as well as p50/p95.
The path is structurally accepted when:

- point and indexed-order work grows logarithmically or remains effectively
  flat for fixed `k`;
- exact scans grow linearly;
- ordered exact scans retain only `O(k)` rows;
- sparse filtered vector work follows `s`, not `N`, apart from a documented
  compact bitmap cost;
- no candidate page re-evaluates an already-consumed prefix;
- joins/sorts either remain within a declared memory bound or spill/fail with a
  controlled operator budget rather than exhausting the process.
