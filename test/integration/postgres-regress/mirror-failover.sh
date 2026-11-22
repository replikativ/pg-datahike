#!/bin/sh
set -eu

mode=${1:-promote}
case "$mode" in promote|diverge) ;; *) echo 'Expected promote or diverge' >&2; exit 2 ;; esac
if test "$#" -gt 0; then shift; fi

PG_MIRROR_PG_BIN=${PG_MIRROR_PG_BIN:-$(pg_config --bindir)}
PG_MIRROR_FAILOVER_DIR=$(mktemp -d "${TMPDIR:-/tmp}/pgdh-ha-probe-XXXXXXXX")
PG_MIRROR_PRIMARY_DIR=$PG_MIRROR_FAILOVER_DIR/primary
PG_MIRROR_STANDBY_DIR=$PG_MIRROR_FAILOVER_DIR/standby
primary_port=${PG_MIRROR_PRIMARY_PORT:-56072}
standby_port=${PG_MIRROR_STANDBY_PORT:-56073}
PG_MIRROR_PRIMARY_URL="jdbc:postgresql://127.0.0.1:$primary_port/postgres?user=mirror_ha"
PG_MIRROR_STANDBY_URL="jdbc:postgresql://127.0.0.1:$standby_port/postgres?user=mirror_ha"
PG_MIRROR_SOURCE_URL="jdbc:postgresql://127.0.0.1:$primary_port,127.0.0.1:$standby_port/postgres?user=mirror_follower&hostRecheckSeconds=1"
export PG_MIRROR_PG_BIN PG_MIRROR_FAILOVER_DIR PG_MIRROR_PRIMARY_DIR PG_MIRROR_STANDBY_DIR
export PG_MIRROR_PRIMARY_URL PG_MIRROR_STANDBY_URL PG_MIRROR_SOURCE_URL

cleanup() {
  result=$?
  if test "$result" != 0; then
    for log in "$PG_MIRROR_FAILOVER_DIR/primary.log" "$PG_MIRROR_FAILOVER_DIR/standby.log"; do
      if test -f "$log"; then tail -20 "$log"; fi
    done
  fi
  for directory in "$PG_MIRROR_STANDBY_DIR" "$PG_MIRROR_PRIMARY_DIR"; do
    "$PG_MIRROR_PG_BIN/pg_ctl" -D "$directory" -m immediate -w stop >/dev/null 2>&1 || true
  done
  rm -rf -- "$PG_MIRROR_FAILOVER_DIR"
}
trap cleanup EXIT

"$PG_MIRROR_PG_BIN/initdb" -D "$PG_MIRROR_PRIMARY_DIR" -U mirror_ha -A trust --no-locale >/dev/null
"$PG_MIRROR_PG_BIN/pg_ctl" -D "$PG_MIRROR_PRIMARY_DIR" -l "$PG_MIRROR_FAILOVER_DIR/primary.log" -w start \
  -o "-p $primary_port -k $PG_MIRROR_FAILOVER_DIR -c listen_addresses=127.0.0.1 -c wal_level=logical" >/dev/null
primary_connection="host=127.0.0.1 port=$primary_port dbname=postgres user=mirror_ha"
"$PG_MIRROR_PG_BIN/pg_basebackup" -D "$PG_MIRROR_STANDBY_DIR" -R -C -S mirror_physical -X stream -d "$primary_connection"
cat >> "$PG_MIRROR_STANDBY_DIR/postgresql.auto.conf" <<EOF
port = $standby_port
listen_addresses = '127.0.0.1'
unix_socket_directories = '$PG_MIRROR_FAILOVER_DIR'
wal_level = 'logical'
hot_standby_feedback = on
wal_receiver_status_interval = '1s'
sync_replication_slots = on
primary_conninfo = '$primary_connection'
EOF
"$PG_MIRROR_PG_BIN/pg_ctl" -D "$PG_MIRROR_STANDBY_DIR" -l "$PG_MIRROR_FAILOVER_DIR/standby.log" -w start >/dev/null
"$PG_MIRROR_PG_BIN/psql" -X -qAt -v ON_ERROR_STOP=1 "$primary_connection" \
  -c "ALTER SYSTEM SET synchronized_standby_slots = 'mirror_physical'" -c 'SELECT pg_reload_conf()' >/dev/null

if test "$#" -eq 0; then
  set -- clojure -M:dev -m datahike.pg.mirror-probe --failover
fi
"$@" "$mode"
