# Benchmark results

pgbench matrix runs of pg-datahike vs real PostgreSQL 17, appended by
`bench/append-results.sh`. Methodology:

- Both servers run on the **same machine**; numbers are only comparable
  within one machine's history.
- pg-datahike runs its **in-memory** store (`clojure -J-Xmx2g -M:server`);
  the reference is a **default-config** real PostgreSQL 17 started by
  `bench/realpg.sh` — deliberately untuned on both sides.
- `-M prepared` (extended protocol with prepared statements) reflects how
  real drivers (JDBC, asyncpg, node-postgres) talk to the server, so it is
  the more representative mode; `-M simple` isolates parse/plan overhead.
- Init is `pgbench -i -I dtgp --no-vacuum` (client-side data generation;
  pg-datahike does not support the server-side `g` step).
- Columns: script, protocol mode, client count, transactions/sec, average
  latency in ms, failed-transaction percentage.

## realpg-smoke — 2026-08-01 13:11 UTC — 2513e4e

Env: PGHOST=127.0.0.1 PGPORT=15499 SCALE=1 DURATION=5 CLIENTS="1 4" MODES="simple prepared" SCRIPTS="tpcb select-only"

```tsv
script	mode	clients	tps	latency_ms	failed_pct
tpcb	simple	1	250.816855	3.987	0.000
tpcb	simple	4	302.333282	13.230	0.000
tpcb	prepared	1	222.108108	4.502	0.000
tpcb	prepared	4	331.151737	12.079	0.000
select-only	simple	1	5732.129631	0.174	0.000
select-only	simple	4	38413.748205	0.104	0.000
select-only	prepared	1	10846.681711	0.092	0.000
select-only	prepared	4	63594.276457	0.063	0.000
```

## scale8-matched — 2026-08-02 — pg-datahike 2668514 + datahike 0.8.1768 vs real PG 17

Env: SCALE=8 (800k accounts), prepared mode, T=20-30s per cell, same
machine, same day. pg-datahike: in-memory store, `clojure -M:server`,
tpcb with `--max-tries=10` (0.000% failed in every cell; retried
transactions are transparent client-side retries on 40001). Real PG:
default config on local NVMe (full fsync durability — pg-datahike's
memory store has no durability either way, so writes are not
apples-to-apples; reads are).

```tsv
script	server	clients	tps	latency_ms
select-only	pg-datahike	1	2816	0.355
select-only	postgresql	1	19194	0.052
select-only	pg-datahike	8	21283	-
select-only	postgresql	8	81554	-
tpcb	pg-datahike	1	103	9.7
tpcb	postgresql	1	419	2.384
tpcb	pg-datahike	4	336	-
tpcb	postgresql	4	719	-
tpcb	pg-datahike	8	277	-
tpcb	postgresql	8	997	-
```

Gap summary: reads 3.8-6.8×, writes 2.1-4.1×. NOTE: the realpg-smoke
section above (scale 1, 5s cells) is NOT comparable to scale-8 numbers —
scale-1 tpcb funnels every transaction through one branch row (max
contention) and its select numbers were taken in simple mode; an earlier
revision of our docs mistakenly compared across these baselines.

## scale8-filestore — 2026-08-02 — same build, konserve file store

Same matrix, `DATAHIKE_STORE_PATH` file backend (fsynced object writes):
select c1 2986 / c8 21636 (≈ memory: warm node cache); tpcb c1 52.7 /
c4 82.2 / c8 83.8, 0.000% failed (commit-IO bound past c4). Store size
1.9G at scale 8. With `DATAHIKE_WRITE_OPT=true` (diff-buf 256 + fused
index roots): store 976M but tpcb 32/51/51 and select c1 1859 — the
write-amplification options target object stores, not local NVMe.
