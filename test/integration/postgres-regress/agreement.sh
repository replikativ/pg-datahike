#!/usr/bin/env bash
# Run every application-facing regression file, gather the outputs, and
# hand them to the measurement.
#
# Each `pg-regress-with-fixtures` run writes into its own timestamped
# directory with only that file's output, so the outputs are collected
# here rather than read in place.
set -u
MODE="${1:-gate}"
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
cd "$ROOT" || exit 1
COLLECTED="$(mktemp -d)"
trap 'rm -rf "$COLLECTED"' EXIT

FILES=$(clojure -M -e '
(require (quote [clojure.string :as str]) (quote [clojure.java.io :as io]))
(load-file "test/integration/postgres-regress/agreement.clj")
(println (str/join " " (agreement/application-facing)))' 2>/dev/null | tail -1)

# The harness bootstraps a database per file inside the long-lived
# server, and the server keeps them: after ninety-odd files it had grown
# to 7GB and starved the machine. Recycle it periodically.
RECYCLE=${AGREEMENT_RECYCLE:-30}
SRVLOG="${SCRATCH:-/tmp}/server.log"
restart_server() {
  pkill -9 -f start_pgwire 2>/dev/null
  sleep 2
  : > "$SRVLOG"
  nohup clojure -M:server >>"$SRVLOG" 2>&1 &
  for _ in $(seq 1 60); do
    sleep 2
    grep -qa "Press Ctrl+C" "$SRVLOG" 2>/dev/null && return 0
  done
  return 1
}

n=0
for f in $FILES; do
  if [ $((n % RECYCLE)) -eq 0 ] && [ "$n" -gt 0 ]; then
    printf "\r[%3d] recycling the server%-14s" "$n" "" >&2
    restart_server || { echo "server did not come back" >&2; exit 1; }
  fi
  n=$((n+1))
  printf "\r[%3d] %-28s" "$n" "$f" >&2
  timeout 300 bb pg-regress-with-fixtures "$f" >/dev/null 2>&1
  R=$(ls -t .internal/pg-regress/*/tests/results/"$f".out 2>/dev/null | head -1)
  [ -n "$R" ] && cp "$R" "$COLLECTED/"
done
echo >&2

clojure -M -e "
(load-file \"test/integration/postgres-regress/agreement.clj\")
(agreement/-main \"$MODE\" \"$COLLECTED\")"
