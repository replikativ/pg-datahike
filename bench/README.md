# Benchmarks

pgbench-based comparison of pg-datahike against a real PostgreSQL 17
instance on the same machine. Three pieces:

| Script              | Purpose                                                        |
|---------------------|----------------------------------------------------------------|
| `realpg.sh`         | start/stop/status a throwaway real-PG 17 on port **15499**     |
| `run-matrix.sh`     | init + run a pgbench matrix against any PG-wire server         |
| `append-results.sh` | run the matrix and append a dated block to `RESULTS.md`        |

## Prerequisites

- PostgreSQL 17 server binaries at `/usr/lib/postgresql/17/bin` (override
  with `PGBIN=`), `pgbench` and `psql` on `PATH`.
- pg-datahike started separately (see below) — the harness never starts it.

## 1. Start the servers

**pg-datahike** (from the repo root; do not run from `bench/`):

```sh
clojure -J-Xmx2g -M:server
```

This listens on ports **15432** (primary) and **15433**. Leave it running in
its own terminal.

**Real PostgreSQL reference** (data dir `bench/.realpg-data`, gitignored;
trust auth, user/db `datahike`, 127.0.0.1 only, default config):

```sh
bench/realpg.sh start     # idempotent: initdb once, start if not running
bench/realpg.sh status
bench/realpg.sh stop
```

## 2. Run the matrix

`run-matrix.sh` is driven entirely by environment variables; `PGPORT` is the
only required one. Defaults: `PGHOST=127.0.0.1`,
`PGUSER=PGDATABASE=PGPASSWORD=datahike`, `SCALE=1`, `DURATION=15`,
`CLIENTS="1 4 8"`, `MODES="simple prepared"`, `SCRIPTS="tpcb select-only"`.

Known-good invocations:

```sh
# pg-datahike
PGPORT=15432 bench/run-matrix.sh > results-datahike.tsv

# real PostgreSQL reference
PGPORT=15499 bench/run-matrix.sh > results-realpg.tsv
```

TSV goes to stdout; a human-readable summary (one line per cell:
`<script> <mode> c<clients>: <tps> tps, <latency> ms avg`) goes to stderr.
Exits non-zero if any cell aborts.

Notes:

- Init uses `pgbench -i -I dtgp --no-vacuum` — client-side data generation,
  because pg-datahike does not support the server-side `g` step. Init step
  failures are warnings; the harness then verifies `pgbench_accounts` has
  `SCALE * 100000` rows and hard-fails if not.
- `tpcb` maps to pgbench's default `tpcb-like` script, `select-only` to
  `-S`; any other value is passed through as a pgbench builtin name.
- `-M prepared` reflects how real drivers (JDBC, asyncpg, node-postgres)
  talk to the server; `-M simple` isolates parse/plan overhead.

## 3. Record results

`append-results.sh <label>` runs the matrix with the current environment and
appends a block (UTC date, git sha, env, TSV) to `bench/RESULTS.md`:

```sh
PGPORT=15499 bench/append-results.sh realpg-baseline
PGPORT=15432 bench/append-results.sh pg-datahike-main
```

Ballpark expectations (scale 1, 1 client, tpcb, simple protocol):
pg-datahike ~70 tps, real PG ~7400 tps.
