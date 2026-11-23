#!/bin/sh
set -eu

probe=--slot-recovery
follower=mirror_recovery_reader
case "${1:-}" in
  --namespaces) probe=$1; follower=mirror_admin; shift ;;
  --streaming) probe=$1; follower=mirror_stream_reader; shift ;;
  --type-lifecycle) probe=$1; follower=mirror_lifecycle_reader; shift ;;
esac

pg_bin=${PG_MIRROR_PG_BIN:-$(pg_config --bindir)}
PG_MIRROR_RECOVERY_DIR=$(mktemp -d "${TMPDIR:-/tmp}/pgdh-mirror-publisher-XXXXXXXX")
data_dir=$PG_MIRROR_RECOVERY_DIR/postgres
port=${PG_MIRROR_RECOVERY_PORT:-56076}
PG_MIRROR_RECOVERY_ADMIN_URL="jdbc:postgresql://127.0.0.1:$port/postgres?user=mirror_admin"
PG_MIRROR_SOURCE_URL="jdbc:postgresql://127.0.0.1:$port/postgres?user=$follower"
export PG_MIRROR_RECOVERY_DIR PG_MIRROR_RECOVERY_ADMIN_URL PG_MIRROR_SOURCE_URL

cleanup() {
  result=$?
  if test "$result" != 0; then
    for log in "$PG_MIRROR_RECOVERY_DIR/postgres.log" "$PG_MIRROR_RECOVERY_DIR"/*-crash.log; do
      if test -f "$log"; then tail -30 "$log"; fi
    done
  fi
  "$pg_bin/pg_ctl" -D "$data_dir" -m immediate -w stop >/dev/null 2>&1 || true
  rm -rf -- "$PG_MIRROR_RECOVERY_DIR"
}
trap cleanup EXIT

"$pg_bin/initdb" -D "$data_dir" -U mirror_admin -A trust --no-locale --encoding=UTF8 >/dev/null
"$pg_bin/pg_ctl" -D "$data_dir" -l "$PG_MIRROR_RECOVERY_DIR/postgres.log" -w start \
  -o "-p $port -k $PG_MIRROR_RECOVERY_DIR -c listen_addresses=127.0.0.1 -c wal_level=logical" >/dev/null

if test "$#" -eq 0; then
  set -- clojure -M:dev -m datahike.pg.mirror-probe "$probe"
fi
"$@"
