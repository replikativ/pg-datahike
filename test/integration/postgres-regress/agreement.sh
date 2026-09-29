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

# A file whose run produced nothing -- a failed bootstrap, a timeout --
# must not be measured from an EARLIER run's leftovers. `ls -t` happily
# picks one, and the number that comes back is then last week's: a
# ratchet that cannot see a regression, and a gain that does not show.
# Each run is required to leave a result newer than this stamp.
STAMP="$COLLECTED/.stamp"
MISSING=""

n=0
for f in $FILES; do
  if [ $((n % RECYCLE)) -eq 0 ] && [ "$n" -gt 0 ]; then
    printf "\r[%3d] recycling the server%-14s" "$n" "" >&2
    restart_server || { echo "server did not come back" >&2; exit 1; }
  fi
  n=$((n+1))
  printf "\r[%3d] %-28s" "$n" "$f" >&2
  touch "$STAMP"
  # `geometry` reads `box_tbl`, which `box` creates -- real PostgreSQL
  # runs the schedule against one database, so it is there. Running the
  # prerequisite first in the same disposable database is all that
  # takes; the fixture runner already accepts several targets.
  #
  # It does not move the number and is not meant to. It stops the
  # residual reporting a harness artifact where a real gap hides:
  # `geometry` blocked on `relation "box_tbl" does not exist` and now
  # blocks on the SAME LINE with `function center(text) does not
  # exist`, which is the thing worth fixing.
  #
  # ONE file, out of 174. I measured before writing this, and then
  # again after, because the first count was wrong:
  #
  #   24 files have a missing relation as their first blocker;
  #   13 are unimplemented CATALOGS (pg_operator, pg_statistic,
  #      pg_ts_parser, information_schema.views, …);
  #    9 are the file's own object, whose CREATE failed earlier;
  #    1 is `with` wanting `q1`, which is a CTE, not a table;
  #    1 is this.
  #
  # `select_views` looked like a second case -- it wants `street` from
  # `create_view` -- but pairing them changes nothing: `CREATE VIEW
  # street` fails on `?#`, an operator we do not parse. Its relation is
  # missing because of us, not because of isolation.
  case "$f" in
    geometry) targets="box $f" ;;
    *)        targets="$f" ;;
  esac
  timeout 300 bb pg-regress-with-fixtures $targets >/dev/null 2>&1
  R=$(find .internal/pg-regress/*/tests/results/"$f".out -newer "$STAMP" \
        2>/dev/null | head -1)
  if [ -n "$R" ]; then
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
