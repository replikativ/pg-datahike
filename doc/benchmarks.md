# Benchmarks

pg-datahike is measured with **stock `pgbench`** — PostgreSQL's own
benchmark tool, unmodified scripts, standard init — against a real
PostgreSQL 17 on the same machine. Nothing here is shape-picked: the
tiers below are pgbench's two built-in workloads across conventional
client counts.

## Results (2026-08-02, scale 8 = 800k accounts, prepared protocol)

| workload | clients | pg-datahike (memory) | pg-datahike (file) | PostgreSQL 17 |
|---|---|---|---|---|
| select-only | 1 | 2,816 tps (0.36 ms) | 2,986 tps (0.34 ms) | 19,194 tps (0.05 ms) |
| select-only | 8 | 21,283 tps | 21,636 tps | 81,554 tps |
| tpcb-like | 1 | 103 tps | 53 tps (19 ms) | 419 tps (2.4 ms) |
| tpcb-like | 4 | 336 tps | 82 tps | 719 tps |
| tpcb-like | 8 | 277 tps | 84 tps | 997 tps |

Gaps vs PostgreSQL: reads 3.8–6.8× (identical for memory and file — the
warm node cache makes the durable store free on reads), writes 2.1–4.1×
from the memory store and 5–12× from the durable file store. Zero failed
transactions in every pg-datahike cell; released artifacts only
(datahike 0.8.1768 from Clojars). PostgreSQL: default configuration on
local NVMe.

**Which write comparison is apples-to-apples?** The file store (konserve:
atomic, fsynced object writes) is the durability-comparable tier for
writes; the memory store shows the transaction-machinery ceiling with
storage taken out. The remaining structural difference on the file tier
is commit shape: a datahike commit writes several objects (index roots +
commit record; the scale-8 dataset occupies ~1.9 GB vs PostgreSQL's
~150 MB) where PostgreSQL appends one WAL record. Datahike's
write-amplification options (`:diff-buf-size`, `:fuse-index-roots?`)
halve the store size but trade commit latency for it on local NVMe —
their target is request-priced object stores (S3).

For context: before the 2026-08 performance campaign, select-only c1 ran
~35× slower than this and the tpcb workload *lost* throughput as clients
were added (optimistic-conflict aborts); writes now scale with
concurrency via PostgreSQL-style row locking.

## Methodology and honest caveats

- **Workloads**: pgbench's built-ins only — `-S` (indexed point reads)
  and the default tpcb-like transaction (3 UPDATEs + SELECT + INSERT in
  an explicit transaction). Init: `pgbench -i -I dtgp --no-vacuum -s 8`
  (client-side generation; the server-side `g` step is unsupported).
- **Protocol**: `-M prepared` — how real drivers (JDBC, asyncpg,
  node-postgres) talk to a server.
- **Retries**: pg-datahike runs tpcb with `--max-tries=10` (a standard
  pgbench flag): its optimistic commit layer can raise serialization
  failures (SQLSTATE 40001) that clients retry, exactly as they must
  against PostgreSQL's serializable modes. Retried transactions are
  counted in the reported tps; the failed percentage was 0.000% in
  every cell.
- **Durability is not apples-to-apples on writes**: pg-datahike's
  in-memory store has no crash durability in any mode, while PostgreSQL
  fsyncs every commit. Read tiers are directly comparable; write tiers
  compare an in-memory transactional store against a durable one.
- **Same machine, same day, both servers untuned.** Single-machine
  numbers are only comparable within one machine's history; run the
  matrix yourself (below) for your hardware.
- Do not compare against numbers taken at other scales: scale-1 tpcb
  funnels all writes through a single branch row and produces very
  different contention behavior on both servers.

## Reproduce

```bash
# pg-datahike (in-memory, port 15432)
clojure -M:server
PGPASSWORD=datahike pgbench -h 127.0.0.1 -p 15432 -U datahike -d datahike \
  -i -I dtgp --no-vacuum -s 8
PGPASSWORD=datahike pgbench -h 127.0.0.1 -p 15432 -U datahike -d datahike \
  -n -S -M prepared -c 1 -T 20
PGPASSWORD=datahike pgbench -h 127.0.0.1 -p 15432 -U datahike -d datahike \
  -n -M prepared --max-tries=10 -c 4 -j 2 -T 30

# reference PostgreSQL 17 (throwaway instance on port 15499)
bench/realpg.sh start
pgbench -h 127.0.0.1 -p 15499 -U datahike -d datahike -i --no-vacuum -s 8
pgbench -h 127.0.0.1 -p 15499 -U datahike -d datahike -n -S -M prepared -c 1 -T 20
```

Raw runs are appended to [bench/RESULTS.md](../bench/RESULTS.md).
