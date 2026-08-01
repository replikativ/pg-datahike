#!/usr/bin/env bash
# Manage a throwaway real-PostgreSQL 17 instance for benchmark comparison.
#
#   bench/realpg.sh start    # initdb (once) + start on port 15499
#   bench/realpg.sh stop     # stop if running
#   bench/realpg.sh status   # report state
#
# Data dir lives in bench/.realpg-data (gitignored). User: datahike, trust
# auth, listens on 127.0.0.1 only. Default config otherwise — the point is
# an honest out-of-the-box reference, not a tuned one.
set -euo pipefail

PGBIN="${PGBIN:-/usr/lib/postgresql/17/bin}"
BENCH_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DATA_DIR="$BENCH_DIR/.realpg-data"
PORT="${REALPG_PORT:-15499}"
PG_USER="${REALPG_USER:-datahike}"
PG_DB="${REALPG_DB:-datahike}"
LOG_FILE="$DATA_DIR/server.log"

pg() { "$PGBIN/$1" "${@:2}"; }

is_running() {
  [ -f "$DATA_DIR/PG_VERSION" ] && pg pg_ctl -D "$DATA_DIR" status >/dev/null 2>&1
}

cmd_start() {
  if [ ! -f "$DATA_DIR/PG_VERSION" ]; then
    echo "realpg: initdb into $DATA_DIR" >&2
    mkdir -p "$DATA_DIR"
    pg initdb -D "$DATA_DIR" -U "$PG_USER" -A trust >/dev/null
  fi
  if is_running; then
    echo "realpg: already running on port $PORT" >&2
    return 0
  fi
  pg pg_ctl -D "$DATA_DIR" -l "$LOG_FILE" -w \
    -o "-p $PORT -c listen_addresses=127.0.0.1 -c unix_socket_directories=''" \
    start >/dev/null
  # Ensure the benchmark database exists (idempotent).
  if ! pg psql -h 127.0.0.1 -p "$PORT" -U "$PG_USER" -d postgres -Atc \
      "SELECT 1 FROM pg_database WHERE datname = '$PG_DB'" | grep -q 1; then
    pg createdb -h 127.0.0.1 -p "$PORT" -U "$PG_USER" "$PG_DB"
  fi
  echo "realpg: running on 127.0.0.1:$PORT (user=$PG_USER db=$PG_DB)" >&2
}

cmd_stop() {
  if is_running; then
    pg pg_ctl -D "$DATA_DIR" -m fast -w stop >/dev/null
    echo "realpg: stopped" >&2
  else
    echo "realpg: not running" >&2
  fi
}

cmd_status() {
  if is_running; then
    echo "realpg: running on 127.0.0.1:$PORT (data dir $DATA_DIR)"
  else
    echo "realpg: not running (data dir $DATA_DIR)"
    return 1
  fi
}

case "${1:-}" in
  start)  cmd_start ;;
  stop)   cmd_stop ;;
  status) cmd_status ;;
  *) echo "usage: $0 {start|stop|status}" >&2; exit 2 ;;
esac
