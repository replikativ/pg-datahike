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
| unindexed scalar `ORDER BY rank LIMIT k` | sequential scan plus bounded top-N heap, `O(N log k)` memory `O(k)` | generic Datahike result construction plus a bounded, stable top-N heap on the JVM, `O(N log k)` memory `O(k)` | Same selection complexity now. Datahike still allocates the upstream relation rather than pulling rows through a demand-driven operator. |
| indexed scalar order | backward B-tree scan stopped by `Limit`, `O(log N + k)` | exact AVET forward/reverse slice when SQL/Datahike ordering is proven compatible; otherwise exact Stratum page and primary recheck | AVET path has the right shape. Stratum one-page top-N is `O(N log k)` today, not B-tree-shaped; OFFSET continuations can repeat that work. |
| unindexed full text | sequential row scan and exact `@@` evaluation | primary tsvector scan and exact PostgreSQL-compatible matcher | Same linear class. Datahike performs attribute-oriented reads rather than one heap-row fetch. |
| GIN/Scriptum full-text filter | posting-list/bitmap combination, heap recheck, then requested SQL order | Lucene posting-list query to an entity bitmap, planner pushdown, authoritative tsvector recheck, then requested SQL order | Same `O(p + candidates)` access class. Entity-filter composition is pushed into Lucene. Ranking remains an exact primary operation. |
| exact vector top-k | sequential distance scan plus bounded top-N heap, `O(N*d + N log k)` | sequential distance evaluation and relation construction followed by pg-datahike's bounded top-N heap | Same distance/selection complexity and `O(k)` selection memory. The much larger constant is now attributable to EAV relation construction, expression evaluation, and JVM allocation—not a full sort. |
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

## Production-path evidence from the current 100k fixture

The benchmark now instruments the optimized path without enabling Datahike's
legacy `:stats?` executor. A matched 100k × 16-dimensional run observed:

| Workload | pg-datahike boundaries | PostgreSQL 17.10 actual plan |
|---|---|---|
| scalar indexed top-10 | Stratum emits 11 to establish continuation state; the candidate contract and final Datahike query each contain 10 | backward B-tree scan emits 10 and touches 3 shared buffers |
| full text, 1% | Scriptum page 1,000 → entity bitmap 1,000 → authoritative query 1,000 | GIN emits 1,000 TIDs → bitmap heap scan 1,000 rows |
| full text, 0.1% | Scriptum page 100 → entity bitmap 100 → authoritative query 100 | GIN emits 100 TIDs → bitmap heap scan 100 rows |
| unfiltered HNSW top-10 | Proximum page 10 → final exact-distance/projection query 10 | HNSW index scan emits 10 |
| filtered HNSW, 10% | one 128-entity ANN set → 18 exact matches → top-10 | HNSW rejects 55 rows and emits 10 |
| filtered vector, 1% | 128-entity ANN set under-fills with 3–4; the fallback projects 1,000 primary IDs once, Proximum exact-scores them, and the final query rechecks 10 | HNSW rejects 738 rows and emits 10 with recall@10 0.8 in this fixture |
| filtered vector, 0.1% | primary range emits 100 → Proximum exact filtered top-10 → final query 10 | the natural planner also chooses primary-key range 100 → bounded exact top-10 → 10 |

This is the key structural result: scalar, full-text, and unfiltered ANN no
longer leak table cardinality across the secondary boundary. The remaining
1%-filtered vector loss is a planner crossover—the engine pays for a bounded
ANN attempt and then performs the exact primary plan that was already cheaper.
PostgreSQL accepts approximate recall on its selected HNSW plan; pg-datahike's
current compatibility path instead guarantees a complete SQL `LIMIT` result.

The clean same-host p50 comparison for that run was:

| Operation | pg-datahike | PostgreSQL 17.10 | Interpretation |
|---|---:|---:|---|
| scalar top-10 through Stratum | 3.21 ms | 0.035 ms | large gap; Stratum chunks are not B-tree-shaped |
| scalar top-10 through experimental AVET backfill | 0.28–0.43 ms | 0.035 ms | right access path; remaining sub-ms runtime/formatting constant |
| full text, 1% / 1,000 matches | 20.24 ms | 0.55 ms | Scriptum 3.25 ms + Datahike/recheck relation 12.65 ms dominate |
| full text, 0.1% / 100 matches | 6.23 ms | 0.108 ms | fixed query/relation setup dominates |
| HNSW top-10, `ef_search=1000` | 1.65 ms | 7.31 ms | Proximum is faster at equal recall@10 1.0 |
| filtered HNSW, 10% | 6.14 ms | 7.42 ms | pg-datahike is faster at equal recall@10 1.0 |
| filtered vector, 1% | 12.90 ms, recall 1.0 | 8.32 ms, recall 0.8 | not semantically equivalent; pg-datahike pays for complete fallback |
| filtered vector, 0.1% / 100 rows | 1.13 ms | 0.112 ms | both choose exact primary-prefilter top-N; same shape, ~1 ms JVM floor |
| exact vector scan | 298 ms | 10.64 ms | largest primary relation/expression constant; not an HNSW problem |
| indexed parent→fact fanout, 100 rows | 2.50 ms | 0.10 ms | runtime AVET parameterization fixes the former full fact scan; Datahike `d/q` is ~0.99 ms |

These are development measurements, not a general engine ranking. The
pg-datahike side calls its in-process query handler while PostgreSQL includes
JDBC result materialization, so the remaining gaps cannot be blamed on wire
round trips. Conversely, Proximum's advantage at this small vector width does
not predict build/query behavior at 384- or 768-dimensional production shapes.

## Cross-cutting structural gaps

1. **Wide-row projection.** PostgreSQL obtains `m` columns from one heap tuple;
   Datahike's fused entity-group executor scans and merges the requested EAVT
   attributes. The measured point path is not `m` independent root seeks: at
   20k rows, one-row projection grew only from 0.77 ms at one column to 1.07 ms
   at sixteen. For a 100-row range it grew from 2.57 to 8.22 ms, versus
   PostgreSQL 0.11 to 0.28 ms. The access complexity is acceptable, but the
   per-entity/per-attribute relation constant is large on multi-row output.
2. **Parameterized indexed joins.** The original indexed 100-row parent→fact
   fanout took 23.1 ms because the second entity group scanned every fact row
   through AEVT and only then merged the bound parent id. Relation execution
   now reselects that group's driver from live upstream bindings and rotates a
   sufficiently selective indexed merge into an AVET/EAVT scan. At 20k rows
   this reduces the full SQL path to 2.50 ms and `d/q` to ~0.99 ms, versus
   PostgreSQL at ~0.10 ms in the final rerun. The access complexity is now
   `O(log N + matches)` on both sides. Remaining work is constant overhead and
   observability: `explain` still prints the static pre-binding driver rather
   than the runtime-selected one.
3. **Demand propagation.** PostgreSQL's pull executor lets `Limit` stop its
   child. Datahike propagates demand for a proven-safe single entity group, but
   post-filters, multiple groups, many joins, aggregation, and generic ordering
   still materialize upstream results.
4. **Sort and spill.** Positive-limit JVM ordering now uses bounded top-N in
   both Datahike and pg-datahike. Unbounded sorts, hash joins, and aggregates
   still lack PostgreSQL's general spill machinery, so equal CPU big-O can
   still become an OOM cliff.
5. **Stratum pagination.** Candidate continuations carry an offset. A later page
   can recompute and discard the prefix; keyset/cursor continuation is required
   before calling this B-tree-equivalent pagination.
6. **Sparse Proximum filters.** Cardinality is now O(1), sparse iteration jumps
   between nonzero words, and filtered HNSW enforces `ef >= k`. Constructing a
   dense entity bitmap still has a compact total-capacity footprint, but the
   scoring/search loop no longer crosses every zero word.

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
