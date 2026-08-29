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
| BRIN summaries | none | next synthetic complete-but-lossy adapter test |
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
| BRIN-style block summaries | individual entity candidates only | semantic fallback is possible but not competitive; needs compact candidate domains |
| vacuum/GC | mark immutable key-maps, fence Konserve writes | Datahike-owned stores covered; external-store root export is still required |

That last GC distinction remains even though every current index uses
Konserve. Scriptum and Stratum generations live in Datahike's store and are
marked directly from retained roots. Proximum currently owns a separate store;
its collector must receive the complete set of generation IDs retained by all
Datahike roots. A common Konserve guard closes the write-before-root race, but
does not by itself tell an external collector which historical roots the owner
still retains.

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

This needs one query-engine addition: an `:external-domain` plan node that
constrains an entity variable without binding it, then intersects that domain
with the primary relation without first materializing every entity ID. It does *not*
need a different publication protocol—the immutable summary generation is
still prepared before, and named by, the same Datahike database root. A
synthetic min/max adapter plus exact no-index differential and range-selectivity
benchmark is the next high-value design test.

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
```

One same-host 20k-row development run (not a release claim) measured:

| Operation | pg-datahike exact | pg-datahike indexed | PostgreSQL 17 indexed |
|---|---:|---:|---:|
| scalar ORDER BY/LIMIT p50 | 17.7 ms | 3.1 ms | 0.13 ms |
| full-text, 10% / 2k matches p50 | 51.3 ms | 34.2 ms | 1.35 ms |
| full-text, 1% / 200 matches p50 | 38.0 ms | 5.7 ms | 0.71 ms |
| full-text, 0.1% / 20 matches p50 | 35.0 ms | 3.3 ms | 0.12 ms |
| vector(384) top-10 p50 | 93.0 ms | 5.2 ms at `ef_search=1000` | not measured locally |

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

The next gains are narrower: avoid general relation construction only where a
candidate contract and a recognized simple SQL shape prove that canonical
recheck/projection remain bounded. Stratum's own top-N query and Proximum
build/filtered-search curves remain adapter-level targets. Full text still
requires PostgreSQL's exact `@@` recheck, and high-cardinality matches still
need the general relation path. Replacing the generation protocol would not
address the dominant read-side cost shown here.

The beta gate is not PostgreSQL parity. It is: no silent false negatives,
useful indexed growth curves, bounded memory/write amplification, and no
unexplained performance cliffs. Profile candidate enumeration, primary
recheck, relation materialization, and wire rendering separately before
changing the storage protocol to chase an end-to-end number.
