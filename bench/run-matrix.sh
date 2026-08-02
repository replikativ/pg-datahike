#!/usr/bin/env bash
# Run a pgbench matrix (scripts x protocol modes x client counts) against any
# PostgreSQL-wire server (pg-datahike or real PostgreSQL).
#
#   PGPORT=15499 bench/run-matrix.sh          # real PG reference
#   PGPORT=15432 bench/run-matrix.sh          # pg-datahike
#
# Env vars:
#   PGPORT      required — server port
#   PGHOST      default 127.0.0.1
#   PGUSER      default datahike
#   PGDATABASE  default datahike
#   PGPASSWORD  default datahike
#   SCALE       default 1     — pgbench scale factor (100k accounts per unit)
#   DURATION    default 15    — seconds per cell
#   CLIENTS     default "1 4 8"
#   MODES       default "simple prepared"   — pgbench -M values
#   SCRIPTS     default "tpcb select-only"  — tpcb = default tpcb-like,
#                                             select-only = pgbench -S
#
# Output: machine-readable TSV on stdout, human-readable table on stderr.
# Init: pgbench -i -I dtgp --no-vacuum (client-side generate; the server-side
# `g` step is unsupported by pg-datahike). Step failures warn, but the row
# count of pgbench_accounts is verified afterwards and mismatch is fatal.
# Exit non-zero if any matrix cell aborts.
set -uo pipefail

: "${PGPORT:?PGPORT is required (e.g. 15432 for pg-datahike, 15499 for real PG)}"
export PGPORT
export PGHOST="${PGHOST:-127.0.0.1}"
export PGUSER="${PGUSER:-datahike}"
export PGDATABASE="${PGDATABASE:-datahike}"
export PGPASSWORD="${PGPASSWORD:-datahike}"

SCALE="${SCALE:-1}"
DURATION="${DURATION:-15}"
CLIENTS="${CLIENTS:-1 4 8}"
MODES="${MODES:-simple prepared}"
SCRIPTS="${SCRIPTS:-tpcb select-only}"

PGBENCH="${PGBENCH:-pgbench}"
PSQL="${PSQL:-psql}"

warn() { echo "run-matrix: $*" >&2; }

# --- init -------------------------------------------------------------------
warn "init: pgbench -i -I dtgp --no-vacuum -s $SCALE ($PGHOST:$PGPORT/$PGDATABASE)"
if ! "$PGBENCH" -i -I dtgp --no-vacuum -s "$SCALE" >&2; then
  warn "WARNING: pgbench init reported failure; verifying row count anyway"
fi

expected=$((SCALE * 100000))
actual="$("$PSQL" -Atc 'SELECT count(*) FROM pgbench_accounts' 2>/dev/null)" || actual=""
if [ "$actual" != "$expected" ]; then
  warn "FATAL: pgbench_accounts has '${actual:-<query failed>}' rows, expected $expected"
  exit 1
fi
warn "init ok: pgbench_accounts has $actual rows"

# --- matrix -----------------------------------------------------------------
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

builtin_for() {
  case "$1" in
    tpcb)        echo "tpcb-like" ;;
    select-only) echo "select-only" ;;
    *)           echo "$1" ;;  # pass through any other pgbench builtin name
  esac
}

printf 'script\tmode\tclients\ttps\tlatency_ms\tfailed_pct\n'
failures=0

for script in $SCRIPTS; do
  builtin_name="$(builtin_for "$script")"
  for mode in $MODES; do
    for c in $CLIENTS; do
      out="$tmpdir/cell.out"
      if ! "$PGBENCH" -n -b "$builtin_name" -M "$mode" \
           -c "$c" -j "$c" -T "$DURATION" >"$out" 2>&1; then
        warn "ABORT: $script $mode c$c — pgbench failed:"
        sed 's/^/    /' "$out" >&2
        printf '%s\t%s\t%s\tABORT\tABORT\tABORT\n' "$script" "$mode" "$c"
        failures=$((failures + 1))
        continue
      fi
      tps="$(sed -n 's/^tps = \([0-9.]*\).*/\1/p' "$out" | head -n1)"
      lat="$(sed -n 's/^latency average = \([0-9.]*\) ms.*/\1/p' "$out" | head -n1)"
      pct="$(sed -n 's/^number of failed transactions: [0-9]* (\([0-9.]*\)%.*/\1/p' "$out" | head -n1)"
      if [ -z "$tps" ] || [ -z "$lat" ]; then
        warn "ABORT: $script $mode c$c — could not parse pgbench output:"
        sed 's/^/    /' "$out" >&2
        printf '%s\t%s\t%s\tABORT\tABORT\tABORT\n' "$script" "$mode" "$c"
        failures=$((failures + 1))
        continue
      fi
      printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$script" "$mode" "$c" "$tps" "$lat" "${pct:-0.000}"
      line="$script $mode c$c: $tps tps, $lat ms avg"
      if [ -n "$pct" ] && [ "$pct" != "0.000" ]; then
        line="$line, $pct% failed"
      fi
      echo "$line" >&2
    done
  done
done

if [ "$failures" -gt 0 ]; then
  warn "$failures cell(s) aborted"
  exit 1
fi
