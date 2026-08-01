#!/usr/bin/env bash
# Run the benchmark matrix with the current environment and append the TSV
# results as a dated, git-sha'd block to bench/RESULTS.md.
#
#   PGPORT=15499 bench/append-results.sh realpg-baseline
#   PGPORT=15432 bench/append-results.sh pg-datahike-main
set -euo pipefail

label="${1:?usage: $0 <label> (env: PGPORT required, see run-matrix.sh)}"

BENCH_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RESULTS="$BENCH_DIR/RESULTS.md"

if [ ! -f "$RESULTS" ]; then
  cat >"$RESULTS" <<'EOF'
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
EOF
fi

tsv="$(mktemp)"
trap 'rm -rf "$tsv"' EXIT

# run-matrix.sh: TSV on stdout (captured), human table on stderr (passes through).
"$BENCH_DIR/run-matrix.sh" >"$tsv"

sha="$(git -C "$BENCH_DIR" rev-parse --short HEAD 2>/dev/null || echo unknown)"
{
  echo
  echo "## $label — $(date -u '+%Y-%m-%d %H:%M UTC') — $sha"
  echo
  echo "Env: PGHOST=${PGHOST:-127.0.0.1} PGPORT=${PGPORT:-?} SCALE=${SCALE:-1}" \
       "DURATION=${DURATION:-15} CLIENTS=\"${CLIENTS:-1 4 8}\"" \
       "MODES=\"${MODES:-simple prepared}\" SCRIPTS=\"${SCRIPTS:-tpcb select-only}\""
  echo
  echo '```tsv'
  cat "$tsv"
  echo '```'
} >>"$RESULTS"

echo "append-results: appended '$label' block to $RESULTS" >&2
