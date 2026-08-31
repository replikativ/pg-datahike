# Secondary-index validation

This is a maintainer checklist for validating Datahike's secondary-index
abstraction against PostgreSQL access-method semantics. It is not a promise
that every PostgreSQL index declaration or extension is supported.

Primary references:

- PostgreSQL `IndexAmRoutine`: `../postgres/src/include/access/amapi.h`
- [PostgreSQL index access methods](https://www.postgresql.org/docs/current/indexam.html)
- [PostgreSQL index method functions](https://www.postgresql.org/docs/current/index-functions.html)
- [PostgreSQL index types](https://www.postgresql.org/docs/current/indextypes.html)
- [pgvector](https://github.com/pgvector/pgvector)

## Core contract under test

An adapter generation is an immutable value named by the Datahike database
root. The adapter may prepare content-addressed objects before commit, but it
must not publish through a mutable native branch or `latest` cell. Committing
the Datahike root is the only visibility point. Konserve GC marks generation
addresses reachable from retained roots.

Candidate pages declare three independent properties:

| Property | Values | Meaning |
|---|---|---|
| precision | `:exact`, `:recheck` | Whether every candidate already satisfies the operator |
| recall | `:complete`, `:approximate` | Whether exhaustion covers every possible match |
| ordering | `:exact`, `:approximate`, `:none` | Strength of the requested candidate order |

Collapsing these into one `exact?` flag is incorrect. Full-text is commonly
`:recheck/:complete/:none`; HNSW is
`:recheck/:approximate/:exact` for its frozen discovered set; scalar Stratum
order is `:exact/:complete/:exact`.

Current pgvector makes the separation especially visible. Its HNSW and
IVFFlat tuple scans set PostgreSQL's `xs_recheck` and `xs_recheckorderby` false:
the distance/order of each returned node is usable as-is. The access method can
still omit nearer corpus members, and iterative scan may advertise strict or
relaxed order. PostgreSQL's two recheck bits do not express that recall axis;
Datahike's explicit `:recall` and continuation metadata must not be collapsed
to imitate them.

These properties are validated by the adapter protocol, but are not yet a
general Datahike physical-plan node. Planner-native entity-filter pushdown
currently goes through `ISecondaryIndex/-search` and `-slice-ordered`.
pg-datahike calls the paged protocol directly for the scalar Stratum top-N
path; Scriptum and Proximum SQL clauses use the planner-integrated search
protocol. A future paged plan node must preserve Proximum's native filtered
search rather than filtering an already frozen ANN page.

## PostgreSQL mapping

| PostgreSQL shape | Validation adapter | Current boundary |
|---|---|---|
| B-tree equality/range/order | native AVET plus opt-in Stratum candidate pages | one non-null scalar column; exact order, range conjunctions, LIMIT/OFFSET |
| Hash equality | native AVET point lookup | no separate physical adapter needed |
| GIN/GiST `tsvector @@ tsquery` | Scriptum candidates plus exact `@@` | constants, `plainto_tsquery`, and `phraseto_tsquery`; complete paging beyond 1,000 matches |
| GiST/SP-GiST KNN | ordered candidate protocol | represented; no general operator-class registry yet |
| pgvector HNSW | Proximum frozen ANN candidate pages | L2, inner product, cosine; exact distance recheck; approximate membership |
| pgvector IVFFlat | none | rejected explicitly |
| BRIN summaries | validated entity-interval domain seam | bounded EAVT reads and exact recheck proven; planner injection awaits operator descriptors |
| GIN arrays/JSONB | none | next Boring-backed extraction experiment |
| expression/partial/multicolumn/INCLUDE | none | needs a planner-visible capability/projection descriptor |

Unique constraints remain a transactor concern. A secondary adapter cannot
claim PostgreSQL uniqueness merely because it can return an ordered or exact
candidate set.

## Access-method capability audit

PostgreSQL's `IndexAmRoutine` separates access-method properties from scan
callbacks. The local `../postgres` checkout is PostgreSQL 19devel; its current
flags include value ordering, operator ordering, equality/hash consistency,
backward scans, uniqueness, multicolumn keys, optional leading keys, array and
NULL search, stored key types, clustering, predicate locks, parallel build and
scan, INCLUDE columns, and summarizing storage. Mapping those properties onto
Datahike exposes the following boundaries:

| Capability dimension | Datahike secondary shape | Assessment |
|---|---|---|
| snapshot visibility | immutable generation key-map in the database root | strong; one root publishes primary and secondary state |
| insert/build/abort | transient builder, prepare, release outcome | covered; concurrent backfill remains lifecycle-tested |
| exact vs lossy match | candidate `:precision` plus primary recheck | represented and validated; planner-native candidate-page recheck is still required |
| complete vs approximate membership | candidate `:recall` | represented and intentionally separate from recheck; not yet planner-costed |
| value or operator order | candidate `:ordering`, `-slice-ordered` | `-slice-ordered` is planner-integrated for one key; paged ordering metadata is not yet planner-consumed |
| bitmap/filter composition | `EntityBitSet`, optional or required upstream filter | covered for entity sets; the required dependency fails closed |
| scan/rescan | opaque page continuation | stable adapter paging covered; no general physical plan node or adaptive planner feedback yet |
| operator classes/strategies | opaque query spec, SQL-side hard-coded mapping | works for current adapters; needs a declarative capability registry |
| multicolumn/expression/partial/INCLUDE | attrs/config can carry metadata | not planner-normalized or executable end to end |
| uniqueness | primary transactor constraint | correctly outside the read adapter; materialized unique secondaries are rejected |
| index-only payload | candidate result columns | representable, but SQL has no visibility/coverage cost rule yet |
| BRIN-style block summaries | optional compact entity-interval domains | representation, validation, temporal fail-closed behavior, and bounded EAVT reads covered; not planner-selected yet |
| vacuum/GC | mark immutable key-maps, fence Konserve writes | Datahike-owned stores covered; external-store root export is still required |

That last GC distinction remains even though every current index uses
Konserve. Scriptum and Stratum generations live in Datahike's store and are
marked directly from retained roots. Proximum currently owns a separate store;
its collector must receive the complete set of generation IDs retained by all
Datahike roots. A common Konserve guard closes the write-before-root race, but
does not by itself tell an external collector which historical roots the owner
still retains.

The present guard also must not be mistaken for a distributed writer lease.
An in-process publication owner is sufficient to stop two local consumers from
taking the same *unpublished* generation, while independent writers may safely
derive distinct immutable children from the same committed root and let the
primary head CAS choose the winner. GC is different: before multi-process or
serverless writers are supported, every process that can write generation
objects or backfill state must publish a durable, expiring store lease (or join
an equivalent fenced GC epoch). A collector must fence new writers and observe
all live writer/build leases before it takes its root snapshot and sweeps.
JVM-local guard membership cannot protect Lambda-style concurrent instances.
Until that protocol lands, deployments must not run storage GC concurrently
with writers outside the documented single-process coordination boundary.

## Next interface falsification: summarizing scan domains

The current candidate protocol is a good fit for access methods that eventually
name tuples: B-tree, GIN/GiST/SP-GiST bitmap scans, and HNSW can all yield entity
IDs with independent match, recall, and ordering guarantees. It should not be
stretched to pretend that every index works that way.

PostgreSQL BRIN stores one summary for a range of heap pages. A matching summary
adds the *whole page range* to a lossy bitmap; the heap scan then visits and
rechecks its tuples. Unsummarized ranges must also be scanned. Expanding those
ranges into tuple IDs inside the secondary would preserve answers but destroy
the representation and I/O advantage that the experiment is meant to test.

A useful Datahike analogue is a second result domain alongside entity candidate
pages:

```clojure
{:domain :entity-intervals
 :intervals [[first-eid last-eid] ...]
 :precision :recheck
 :recall :complete
 :ordering :none
 :continuation ...}
```

Fixed logical entity-ID intervals are preferable to persistent-tree node
addresses for the first experiment: they survive node rebalancing, can be
searched as bounded EAVT ranges, and tend to retain insertion correlation for
SQL table rows. They are not an imitation of PostgreSQL heap blocks; they are
the stable primary-scan partition available in Datahike's model. Each interval
would store min/max (then bloom or multi-minmax) summaries for configured
attributes. Inserts widen a summary, deletion may leave it conservatively
loose, and compaction can tighten it. Missing or unsummarized intervals are
always returned, so failure costs performance rather than recall.

The Datahike branch now has an optional `ISecondaryCandidateDomain` contract,
strict interval validation, lifecycle/temporal gates, and a physical EAVT seam
that reads one bounded slice per interval. A synthetic min/max domain containing
false positives was rechecked against canonical values and matched the exact
no-index answer on every primary backend. In a same-JVM 50k-entity REPL probe,
forcing datom value reads took a median 0.55 ms for the full attribute, 0.17 ms
for one 10% interval, 0.03 ms for one 1% interval, and 0.12 ms for ten disjoint
ranges totaling 1%. These tiny in-memory figures are not a benchmark claim;
they establish that work follows selected entities plus range-seek count rather
than expanding the domain into a bitmap.

The experiment deliberately did **not** expose an ad hoc `:external-domain`
query function. A lossy domain is sound only when the planner can prove that
the corresponding canonical predicate will recheck it. Merely sharing an
entity variable with some later pattern cannot prove that relationship. The
planner-visible producer must therefore be an operator descriptor which owns
both summary translation and exact primary lowering; otherwise a user or SQL
adapter could silently omit the recheck.

That future plan node constrains an entity variable without binding it, then
intersects the domain with the primary relation without materializing every
entity ID. It does *not* need a different publication protocol—the immutable
summary generation is still prepared before, and named by, the same Datahike
database root.

Intervals should be sorted, disjoint, half-open, and normalized by merging
adjacent ranges. EAVT can then issue one bounded slice per interval while the
ordinary predicate remains the authoritative recheck. Stratum can prototype
this from its aligned entity-id column and existing chunk zone maps. That first
version will still inspect every chunk header: persistent-sorted-set exposes
aggregate measures but not yet a measure-aware subtree visitor returning key
ranges. A later `walk-measured-ranges` operation would let Stratum prune whole
subtrees from merged statistics before loading descendant chunks.

The other PostgreSQL families divide cleanly after that experiment:

- Hash equality already maps to Datahike AVET point lookup; another physical
  adapter would add little semantic coverage.
- GIN arrays and JSONB reuse complete/recheck inverted candidates, but require
  typed extraction and an operator-class registry rather than Lucene text
  analysis.
- General GiST/SP-GiST and multicolumn B-tree need declarative operator-family,
  key projection, partial-predicate, and ordering capabilities. The immutable
  generation and candidate contracts do not need to change.
- `INCLUDE` and index-only scans need candidate payload columns plus a planner
  rule that proves the payload belongs to the current immutable generation;
  they are a performance extension, not a matching-semantics extension.

## Next interface falsification: operator descriptors

The SQL layer currently recognizes each supported secondary family directly,
while Datahike external-engine functions carry opaque query specs. That was
enough to exercise Scriptum, Proximum, and Stratum, but it is not enough to
select a lossy summarizing domain safely or to add PostgreSQL-shaped operator
families without duplicating semantic decisions.

One immutable descriptor should normalize these dimensions:

- operator family and strategy identity, including NULL/empty-value behavior;
- key projection (one or more attributes, later a deterministic expression);
- predicate-to-adapter query translation;
- exact primary predicate lowering used for recheck;
- result domain (`:entities`, ordered rows with payload, or entity intervals);
- precision, recall, ordering, and continuation capabilities;
- partial-index predicate implication and included payload columns;
- cardinality/startup/per-result estimates.

The critical rule is that candidate generation and recheck lowering come from
the same descriptor. pg-datahike may map PostgreSQL operator classes onto these
descriptors, but it must not privately reinvent their correctness semantics.
Datahike can then use the same descriptors from Datalog and, later, an async
CLJS executor. This is also the natural place to decide whether a result can be
index-only, whether an ANN continuation may stop, and whether a complete range
domain is legal in an exact query.

## Required semantic waves

1. Lifecycle: empty/populated build, concurrent writes during backfill,
   failure/abort, restart, branch/history restoration, dump/restore, purge,
   and GC marking.
2. Full-text: AND/OR/NOT, prefix, weights, phrase distance, empty query,
   cardinality-many values, more than 1,000 matches, and bitmap composition.
   Every indexed result is compared with the exact no-index SQL result.
3. Vector: each operator class, dimensions/options, NULL and cosine-zero
   behavior, deterministic recall@k, filters at 100/10/1/0.1 percent, updates,
   deletes, branches, and restart.
4. New shapes: Stratum range/order, a BRIN-like range-summary adapter, then
   JSONB GIN extraction. These are interface tests, not only features.

Filtered ANN requires special care. PostgreSQL/pgvector normally applies an
ordinary filter after an approximate scan and can expand that scan iteratively.
Datahike can now make an external engine depend on an upstream entity relation
and pass the resulting bitmap into Proximum's native filtered search. The
contract is explicit: accepting a filter and requiring one are distinct, and a
required filter fails closed if no producer ran. pg-datahike still retries the
exact query if an approximate filtered result under-fills SQL LIMIT. Adaptive
continuations remain the next step for filters which cannot be cheaply
materialized before ANN.

`DROP INDEX` is an atomic declaration/root transition: the new database value
omits the generation and retained historical values keep it. `DROP TABLE`
cascades materialized declarations in the same transaction when the covered
attributes can be removed. Dropping and recreating a *populated* SQL table
while Datahike retains history is a separate open schema-identity problem:
reusing the same keyword for a different PostgreSQL relation generation would
make old datoms acquire the new schema. The secondary work must not bypass
Datahike's guard against that unsafe transition.

## Reproducible probes

With the corresponding local Datahike, Scriptum, Proximum, and Stratum branches
checked out on JDK 22 or newer:

```bash
clojure -J-Xmx2g -M:local-secondary-stack:test \
  --focus datahike.test.pg-secondary-validation-test

SECONDARY_BENCH_ROWS=20000 SECONDARY_BENCH_DIMENSION=384 \
  SECONDARY_BENCH_EF_CONSTRUCTION=200 \
  clojure -J-Xmx6g -M:dev:local-secondary-stack \
  bench/secondary_validation.clj

bench/realpg.sh start
SECONDARY_BENCH_ROWS=20000 \
  clojure -M:dev bench/postgres_secondary_reference.clj

SECONDARY_BUILD_ROWS=10000,100000 \
  clojure -J-Xmx4g -M:dev:local-secondary-stack \
  bench/scriptum_build_growth.clj
```

One same-host 20k-row development run (not a release claim) measured:

| Operation | pg-datahike exact | pg-datahike indexed | PostgreSQL 17 indexed |
|---|---:|---:|---:|
| scalar ORDER BY/LIMIT p50 | 17.7 ms | 3.1 ms | 0.13 ms |
| full-text, 10% / 2k matches p50 | 51.3 ms | 34.2 ms | 1.35 ms |
| full-text, 1% / 200 matches p50 | 38.0 ms | 5.7 ms | 0.71 ms |
| full-text, 0.1% / 20 matches p50 | 35.0 ms | 3.3 ms | 0.12 ms |
| vector(384) top-10 p50 | 93.0 ms | 5.2 ms at `ef_search=1000` | see matched pgvector run below |

At `ef_search=1000`, a 12-query deterministic vector sample measured mean
recall@10 0.992, minimum 0.9. The fixed `[1,0,...]` probe was the minimum; the
default `ef_search=40` found only one of its ten exact neighbors, so default
quality is not release-ready for this high-dimensional shape. Filter-aware ANN
had complete recall in the two probes but remained slower than the already
selective exact scan (35.7 vs 29.3 ms at 10%, 20.9 vs 15.5 ms at 1%). That is a
real crossover to retain, not hide. Build time was about 1.65 s for Stratum,
0.57 s for Scriptum, and 18.7 s for Proximum at 384 dimensions and
`ef_construction=200`, versus 10.7 ms for PostgreSQL B-tree and 6.6 ms for GIN
on this small corpus.

A separate 10k-row, 16-dimensional diagnostic run after collapsing the SQL
path to one `datahike.api/q` call per statement located the remaining time more
precisely. Stratum's `q` accounted for 2.7 ms of a 2.75 ms candidate page and a
4.6 ms scalar SQL query. Scriptum's native candidate page accounted for 8.3 ms
of a 26.8 ms Datahike query and a 46 ms SQL query at 1,000 matches; at 10
matches those figures fell to 1.1, 5.3, and 6.2 ms. Unfiltered Proximum search
accounted for about 2.0 ms of a 5.6 ms Datahike query and 6.5 ms SQL query at
`ef_search=1000`. For filtered ANN, however, Proximum used only 7.5 ms of a
55 ms Datahike query at 10% selectivity, and the exact 36.5 ms scan still won.

These are inclusive stage timings, not additive components, but the direction
is stable: the native engines are useful and no longer hidden behind repeated
query dispatch. A first bounded physical split now materializes an unfiltered
Proximum candidate page outside the general external-engine planner, then
passes only those entity IDs to the existing authoritative Datalog distance,
ordering, and projection query. On the same live 10k-row REPL fixture at
`ef_search=1000`, this reduced top-10 p50 from 8.65 ms through the generic
external-engine route to 4.19 ms. Filtered ANN deliberately stays in the
planner so an upstream `EntityBitSet` reaches Proximum.

Full-text planning now asks Scriptum for an exact hit count against the same
immutable snapshot before making the access-path decision. A same-JVM 10k-row
probe measured the 10%-selective candidate path at 58.7 ms versus 19.9 ms for
the primary scan, while the maintained benchmark continued to show clear
secondary wins at 1% and 0.1%. The initial conservative gate therefore retains
Scriptum for at most twenty percent of a table (or 64 absolute hits) and
otherwise keeps the exact primary path. The benchmark records the count call
separately; the >1,000-hit continuation test explicitly forces Scriptum because
it tests adapter completeness rather than planner choice.

The next gains are narrower: avoid general relation construction only where a
candidate contract and a recognized simple SQL shape prove that canonical
recheck/projection remain bounded. Stratum's own top-N query and Proximum
build/filtered-search curves remain adapter-level targets. Full text still
requires PostgreSQL's exact `@@` recheck, and high-cardinality matches still
need the general relation path. Replacing the generation protocol would not
address the dominant read-side cost shown here.

### Matched pgvector HNSW reference

With pgvector 0.8.0 installed into the same PostgreSQL 17.10 instance, a
matched 20k-row, 384-dimensional run used the identical generated vectors and
query sample, cosine distance, `m=16`, `ef_construction=200`, and
`ef_search=1000`:

| Operation | pg-datahike / Proximum | PostgreSQL / pgvector |
|---|---:|---:|
| HNSW build | 55.7 s | 20.4 s |
| Exact top-10 p50 | 282.5 ms | 16.1 ms |
| HNSW top-10 p50 | 8.4 ms | 39.3 ms |
| 12-query mean/min recall@10 | 0.975 / 0.9 | 0.983 / 0.9 |
| 10%-filtered exact / HNSW p50 | 91.8 / 153.2 ms | 11.0 / 39.1 ms |
| 1%-filtered exact / HNSW p50 | 6.2 / 35.9 ms | 10.2 / 37.2 ms |

This is a bounded development run, not a general engine ranking. It does show
three different costs that must not be collapsed into “vector performance”:
Proximum's query graph is competitive, its current build path is not yet, and
pg-datahike's exact/recheck path has a much larger cliff than PostgreSQL's. At
the two filtered probes Proximum returned all ten exact neighbors, while
pgvector returned ten with recall 0.9. The isolation below shows that initial
construction, unlike descendant updates, is not dominated by a generation
mmap copy.

pgvector 0.8.0 also demonstrates why filtered ANN needs an explicit
continuation contract. With `ef_search=40` and a 1% post-filter, its default
`hnsw.iterative_scan=off` visited forty candidates and returned zero rows in a
22 ms `EXPLAIN ANALYZE` probe. `strict_order` continued until it found ten rows,
touching 726 rejected candidates and taking 60 ms. Proximum's candidate cursor
and Datahike's `ISecondaryCandidateScan` have the right semantic shape for this;
the remaining work is a planner-controlled breadth/continuation policy and a
bounded authoritative recheck, not a different generation model.

### Proximum layer isolation

A warm in-process REPL probe on 10k deterministic 384-dimensional vectors,
using cosine distance, `m=16`, and `ef_construction=200`, separated the same
stack into native, Datahike, and SQL work. These figures are diagnostic rather
than a cross-process benchmark:

| Query stage, `ef_search=1000` | p50 |
|---|---:|
| Proximum native top-10 search | 4.68 ms |
| Proximum candidate cursor | 3.90 ms |
| Datahike validated candidate page | 4.16 ms |
| pg-datahike indexed SQL, including exact distance recheck | 8.36 ms |
| pg-datahike exact scan without HNSW | 136.65 ms |

The secondary protocol and `EntityBitSet` boundary add little to an unfiltered
search. About 2.6 ms of an instrumented SQL query was the authoritative
Datahike recheck over ten candidates; parsing, lowering, and result production
accounted for the remaining small fixed cost. The useful optimization target
is therefore not a pg-datahike-native vector representation on this path.

Initial index construction took 21.96 s end to end. Reading the 10k vectors
from Datahike took 39.9 ms, creating the rootless builder 12.6 ms, inserting
them one at a time into HNSW 20.43 s, and sealing 85.6 ms. More than 90 percent
of the build is graph construction. The mmap was 2 GiB logical but only about
15 MiB allocated, and a descendant-generation fork took about 20 ms on this
sparse-copy-capable filesystem. The fixed logical capacity and shelling out to
`cp` remain portability and bounded-space concerns, but they do not explain
the initial-build gap measured here.

Proximum already contains a transient `insert-batch` path which the Datahike
adapter does not use. The same 10k fixture measured:

| Construction path | Time |
|---|---:|
| current per-datom generation `put!` | 20.43 s |
| one deterministic, single-thread batch | 16.64 s |
| one eight-way batch | 8.15 s |
| streaming 1,000-vector eight-way batches | 6.95–8.06 s |

The streaming result means Datahike need not accumulate an unbounded backfill
to benefit. At `ef_search=400` the per-row, one-batch, and streaming graphs all
had recall@10 0.8 for the fixed hard query; at `ef_search=1000` all reached
1.0. That one query is not enough to approve parallel construction, however:
Proximum documents the parallel neighbor-selection races as nondeterministic.
A production bulk protocol must either make the parallel graph reproducible or
declare and test that secondary generation bytes may vary while query semantics
and the published immutable root remain sound.

Warm single-row SQL vector updates measured 195, 136, and 113 ms. The median
call spent about 19 ms forking the source generation, 3 ms deleting the old
node, 5 ms inserting the new node, 5 ms sealing, and 69 ms reopening the just
sealed generation. A sealed generation is already immutable and queryable.
An explicit ownership transfer from `SealedGeneration` to a ref-counted
`GenerationView` can remove that cold reopen without changing publication:
the GC guard is still released only after Datahike commits the generation ID in
its root, and abort still closes the unpublished handle.

Filter translation has a separate density cliff. Turning external Datahike
entity IDs into Proximum internal IDs by one persistent-sorted-set lookup per
ID cost 0.21, 2.55, 9.43, and 15.19 ms for 100, 1k, 5k, and 10k IDs. Scanning
the numeric external-ID range once and building `ArrayBitSet` directly stayed
between 3.25 and 3.93 ms. Proximum should choose point lookup for sparse sets,
a sorted merge/range scan for dense sets, and ordinary unfiltered HNSW when the
filter covers the corpus. The API should accept a primitive iterator/bitmap so
this optimization is not coupled to Datahike's concrete bitmap type.

Finally, Datahike's raw AVET range scan produced 100, 1k, 5k, and 10k entity
IDs in 0.09, 0.18, 0.55, and 0.67 ms. The corresponding SQL range predicate
still went through a generic relation and erased that advantage. SQL should
lower recognized scalar ranges to an AVET-backed entity-set producer, preserve
its cardinality, and cost exact filtered distance against ANN. On this fixture
exact search already won at one percent selectivity; forcing ANN for every
filter would make a correct index predictably slower.

### Current matched engine-boundary checkpoint

After the bounded scalar projection, direct Scriptum recheck, materialized ANN
probe, and selective-equality vector lanes, a hot 10k-row/16-dimensional run on
the same machine produced the following medians. The pg-datahike column invokes
its query handler in-process; PostgreSQL 17.10 is reached through JDBC, so this
isolates engine/access-path shape rather than claiming a wire-level product
ranking.

| Operation | pg-datahike | PostgreSQL 17 | Ratio |
|---|---:|---:|---:|
| scalar descending top-10 | 0.094 ms | 0.028 ms | 3.4x |
| full-text 10% / 1,000 matches | 1.109 ms | 0.309 ms | 3.6x |
| full-text 1% / 100 matches | 0.206 ms | 0.077 ms | 2.7x |
| full-text 0.1% / 10 matches | 0.123 ms | 0.034 ms | 3.6x |
| HNSW top-10, `ef_search=200`, recall@10 0.9 | 0.564 ms | 0.564 ms | 1.0x |
| HNSW top-10, `ef_search=400`, recall@10 1.0 | 0.664 ms | 1.096 ms | 0.6x |
| vector + 10% filter | 1.797 ms | 0.383 ms | 4.7x |
| vector + 1% unindexed equality | 0.817 ms | 0.292 ms | 2.8x |
| vector + 0.1% indexed range | 0.154 ms | 0.041 ms | 3.7x |
| one vector update | 9.712 ms | 2.831 ms | 3.4x |

The former 1%-filter cliff was about 12.2 ms. Its Proximum work was only
0.35 ms; two generic Datalog passes materialized the equality relation and then
rechecked top-k. The replacement scans the unindexed scalar attribute once,
uses the already-normalized seek value to form entity candidates, and feeds the
existing bounded primary vector evaluator. That evaluator independently
rechecks the canonical `seek-key` binding, every remaining SQL predicate, row
membership, exact distance, NULL order, and projection. A bad selectivity
estimate can therefore choose a slower scan but cannot change the answer.

The query/read target is now met for these matched, acceptable-recall cases.
The first CREATE INDEX numbers included a 20 ms readiness polling quantum.
Waiting on the local connection's immutable-root watch instead (with polling
retained for non-watchable connections) reduced repeated hot 10k-row Stratum
builds to 11.7--19.8 ms, versus 6.1 ms for PostgreSQL B-tree. Scriptum remained
95--144 ms versus 4.6 ms for GIN. Proximum took 467--580 ms versus pgvector's
1.88 s at this small 16-dimensional shape.

Opt-in stage accounting confirms that the generic backfill is already one
batch, not one generation per datom. Stratum accumulates all cells and persists
once. Scriptum adds all 10k documents through one Lucene writer and seals once;
in a hot instrumented build, document ingestion accounted for about 28 ms and
the actual seal for about 10 ms of a roughly 102 ms statement. That preliminary
split did not account reliably for JIT convergence and per-add instrumentation;
the matched 100k direct-API experiment below is the stronger attribution.
Instrumentation wraps every add and is therefore diagnostic rather than a
headline result.

The follow-up 10k/100k growth and allocation probe found two accidental costs.
The adapter computed `secondary-only-hash` for ordinary, primary-backed values,
and its generic five-field document map stored a duplicate value, attribute,
hash, and key for every candidate. `hasch.core/uuid` took 316--398 ms for 100k
distinct strings in the same REPL; `hasch.fast/uuid` took 24--25 ms. Changing
the hash function is nevertheless a persistent-format migration because
`:db.secondary/only` primary datoms contain the old, currently unversioned hash.
The safe fix is to avoid hashing where it has no semantic consumer and retain
the stable hash for authoritative secondary-only data until a versioned/dual
lookup migration exists.

The SQL full-text index now declares a cardinality-one, candidate-only layout.
It stores only the indexed tsvector text and retrievable entity ID, deletes by
that ID, and uses a public Scriptum pre-built-document entry point rather than
decoding nested Clojure field maps per row. Datahike remains authoritative and
performs PostgreSQL `@@` recheck. Multiple attributes, cardinality-many values,
and secondary-only values fail closed instead of selecting this layout.

On the performance CPU governor, the 100k build fell from roughly 759--784 ms
to 127--140 ms; PostgreSQL 17 GIN was 22.96 ms (about 5.5x at the best stable
hot point). Under the powersave governor, matched hot runs measured 276.9--279.1
ms versus PostgreSQL's 39.9--41.9 ms. A direct Scriptum build with the same two
Lucene fields took 278.9 ms (211.3 ms ingestion and 67.6 ms sealing), while the
Datahike AEVT scan took 0.5--1.7 ms. The governor changes absolute times and
JIT convergence, but both comparisons locate the remaining build gap inside
Scriptum/Lucene document ingestion and sealing, not in Datahike traversal,
publication polling, or one-generation-per-datom behavior.

Stratum and Proximum therefore no longer show a build-path structural cliff,
and Scriptum no longer justifies a new Datahike bulk protocol from this
evidence. Its build latency is still materially farther from PostgreSQL than
its query latency, though online backfill does not block the writer. Two direct
follow-ups falsified reparsing as the explanation: indexing normalized text
through the analyzer took 287.1 ms, statistically indistinguishable from the
canonical 278.9 ms path, while emitting repeated exact-term fields took
438.7 ms. A PostgreSQL-specific pre-tokenized representation would therefore
add semantic and storage complexity without a measured build benefit. The
remaining work belongs in Scriptum/Lucene profiling and tuning; none of these
findings call for changing the generic secondary protocol or immutable
generation publication.

The beta gate is not PostgreSQL parity. It is: no silent false negatives,
useful indexed growth curves, bounded memory/write amplification, and no
unexplained performance cliffs. Profile candidate enumeration, primary
recheck, relation materialization, and wire rendering separately before
changing the storage protocol to chase an end-to-end number.
