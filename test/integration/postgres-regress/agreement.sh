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
# Keeping the outputs is the point of the run: the measurement is one
# number, and the 174 files behind it are what says WHERE the residual
# is. A mktemp with a cleanup trap threw them away every time, so the
# analysis meant running the gate again. Set AGREEMENT_KEEP to a
# directory to keep them.
COLLECTED="${AGREEMENT_KEEP:-$(mktemp -d)}"
mkdir -p "$COLLECTED"
if [ -z "${AGREEMENT_KEEP:-}" ]; then
  trap 'rm -rf "$COLLECTED"' EXIT
else
  echo "Collecting outputs in $COLLECTED (kept)" >&2
fi

# AGREEMENT_FILES restricts the run to named files. `agreement.clj`
# accepts it for `probe` only: `measure` rewrites the manifest from what
# it measured, so a subset would drop every other file's baseline.
if [ -n "${AGREEMENT_FILES:-}" ]; then
  if [ "$MODE" != "probe" ]; then
    echo "AGREEMENT_FILES is only supported with 'probe'" >&2
    exit 2
  fi
  # Validate the names BEFORE running anything: agreement.clj also
  # checks, but it runs after the file loop, so a typo there costs a
  # whole run and a started server.
  FILES=$(printf '%s' "$AGREEMENT_FILES" | tr ',' ' ')
  if ! clojure -M -e "
(require (quote [clojure.string :as str]))
(load-file \"test/integration/postgres-regress/agreement.clj\")
(let [in-scope (set (agreement/application-facing))
      asked (str/split \"$FILES\" #\"\\s+\")
      bad (remove in-scope (remove str/blank? asked))]
  (when (seq bad)
    (binding [*out* *err*]
      (println \"not in scope (see scope.edn):\" (str/join \" \" bad)))
    (System/exit 2)))" 2>&1; then
    exit 2
  fi
else
FILES=$(clojure -M -e '
(require (quote [clojure.string :as str]) (quote [clojure.java.io :as io]))
(load-file "test/integration/postgres-regress/agreement.clj")
(println (str/join " " (agreement/application-facing)))' 2>/dev/null | tail -1)
fi

# The harness bootstraps a database per file inside the long-lived
# server, and the server keeps them: after ninety-odd files it had grown
# to 7GB and starved the machine. Recycle it periodically.
RECYCLE=${AGREEMENT_RECYCLE:-30}
SRVLOG="${SCRATCH:-/tmp}/server.log"
# A cold JVM boot here is minutes, not seconds, and it competes with
# whatever else is on the machine: a 120s deadline killed a measure at
# file 90 of 155 two seconds before the server announced itself. The
# deadline is generous on purpose -- the cost of waiting is a slower
# run, the cost of giving up early is losing the whole measurement.
BOOT_TRIES=${AGREEMENT_BOOT_TRIES:-150}
restart_server() {
  for attempt in 1 2; do
    pkill -9 -f start_pgwire 2>/dev/null
    sleep 2
    : > "$SRVLOG"
    nohup clojure -M:server >>"$SRVLOG" 2>&1 &
    for _ in $(seq 1 "$BOOT_TRIES"); do
      sleep 2
      grep -qa "Press Ctrl+C" "$SRVLOG" 2>/dev/null && return 0
    done
    echo "server did not announce itself within $((BOOT_TRIES * 2))s (attempt $attempt); last log lines:" >&2
    tail -5 "$SRVLOG" >&2
  done
  return 1
}

# A file whose run produced nothing -- a failed bootstrap, a timeout --
# must not be measured from an EARLIER run's leftovers. `ls -t` happily
# picks one, and the number that comes back is then last week's: a
# ratchet that cannot see a regression, and a gain that does not show.
# Each run is required to leave a result newer than this stamp.
STAMP="$COLLECTED/.stamp"
MISSING=""
TIMEDOUT=""

# The recycle below is the only thing that ever started a server, so a
# run begun without one measured NOTHING until the first recycle -- 30
# files, silently, reported only as "NO OUTPUT from:" at the very end.
# Start one up front if the port is dead.
server_up() { (exec 3<>/dev/tcp/127.0.0.1/15432) 2>/dev/null; }

# `probe` exists to measure a change you just made, so REUSING a live
# server defeats its whole purpose: the one a previous run left behind
# (nothing here stops it at the end) is running the code from before the
# edit. That is not a hypothetical -- a 4-file probe silently reused a
# 52-minute-old server and reported +0.0 for a fix whose own unit tests
# passed, because the measured server predated it.
#
# `measure` and `gate` keep reusing a warm server: they are run against
# a tree that is not being edited, and a cold JVM boot per run is
# minutes.
if [ "$MODE" = "probe" ]; then
  echo "probe: restarting the server so it carries the current tree" >&2
  restart_server || { echo "server did not come back" >&2; exit 1; }
elif ! server_up; then
  echo "no server on 15432; starting one" >&2
  restart_server || { echo "server did not come back" >&2; exit 1; }
fi

n=0
for f in $FILES; do
  if [ $((n % RECYCLE)) -eq 0 ] && [ "$n" -gt 0 ]; then
    printf "\r[%3d] recycling the server%-14s" "$n" "" >&2
    restart_server || { echo "server did not come back" >&2; exit 1; }
  fi
  n=$((n+1))
  printf "\r[%3d] %-28s" "$n" "$f" >&2
  touch "$STAMP"
  # Run fixture-producing predecessors before their consumers, in the
  # pinned schedule's order and in the same disposable database. Only
  # the target file's output is collected below. test_setup already
  # provides point_tbl; the other geometry and datetime fixtures come
  # from the explicit predecessors here. A prerequisite can itself fail;
  # its consumer's remaining errors then describe that compatibility gap.
  case "$f" in
    geometry)     targets="lseg line box path polygon circle $f" ;;
    horology)     targets="date time timetz timestamp timestamptz interval $f" ;;
    join)         targets="create_table create_misc $f" ;;
    select_views) targets="create_view $f" ;;
    numeric_big)  targets="numeric $f" ;;
    *)            targets="$f" ;;
  esac
  # A file killed by this limit still leaves a PARTIAL .out, newer than
  # the stamp, so collecting it would measure how fast this machine is.
  # `alter_table`'s last write was exactly 300s after its run started,
  # and its 8.8 points of "regression" were the cut moving, not us.
  # `timeout` exits 124 when it fires; that file is NOT measured.
  timeout "${AGREEMENT_FILE_TIMEOUT:-900}" bb pg-regress-with-fixtures $targets >/dev/null 2>&1
  RC=$?
  # A server that DIES mid-file is worse than one that times out: the
  # file leaves a partial .out that is collected and measured, and
  # every file after it finds no server at all until the next recycle.
  # It cost a whole run: `foreign_key` was measured at 6.1% against a
  # 66.7% baseline and the four files after it produced nothing, and
  # none of that was a regression. Notice it, restart, and try once
  # more -- the retry is what makes the number mean something.
  if ! server_up; then
    printf "\r[%3d] %-28s server died; restarting\n" "$n" "$f" >&2
    restart_server || { echo "server did not come back" >&2; exit 1; }
    touch "$STAMP"
    timeout "${AGREEMENT_FILE_TIMEOUT:-900}" bb pg-regress-with-fixtures $targets >/dev/null 2>&1
    RC=$?
    if ! server_up; then
      echo "server died again during $f; not measuring it" >&2
      RC=124
    fi
  fi
  R=$(find .internal/pg-regress/*/tests/results/"$f".out -newer "$STAMP" \
        2>/dev/null | head -1)
  if [ "$RC" -eq 124 ]; then
    TIMEDOUT="$TIMEDOUT $f"
    MISSING="$MISSING $f"
  elif [ -n "$R" ]; then
    cp "$R" "$COLLECTED/"
  else
    MISSING="$MISSING $f"
  fi
done
rm -f "$STAMP"
echo >&2

# Say it BEFORE measuring, and again after, but measure either way: the
# run costs an hour and a half, and throwing that away over one file
# that timed out helps nobody. What must not happen is measuring the
# missing file from an earlier run's leftovers, and it no longer can --
# it is simply absent from the collected outputs.
if [ -n "$TIMEDOUT" ]; then
  echo "TIMED OUT (${AGREEMENT_FILE_TIMEOUT:-900}s):$TIMEDOUT" >&2
  echo "A killed run leaves a partial .out; measuring it would report" >&2
  echo "this machine's speed as agreement. Raise AGREEMENT_FILE_TIMEOUT." >&2
fi

if [ -n "$MISSING" ]; then
  echo "NO OUTPUT from:$MISSING" >&2
  echo "Those files produced no result this run. They are NOT measured" >&2
  echo "below. Check the newest .internal/pg-regress/*/bootstrap.log." >&2
fi

clojure -M -e "
(load-file \"test/integration/postgres-regress/agreement.clj\")
(agreement/-main \"$MODE\" \"$COLLECTED\")"
STATUS=$?

if [ -n "$MISSING" ]; then
  echo >&2
  echo "REMINDER -- no output this run from:$MISSING" >&2
  exit 1
fi
exit "$STATUS"
