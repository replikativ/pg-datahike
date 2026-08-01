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
