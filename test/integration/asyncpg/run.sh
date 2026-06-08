#!/usr/bin/env bash
# Run the asyncpg wire-protocol regression suite against Datahike pgwire.
#
# Requirements:
#   - setup.sh has been run
#   - Datahike pgwire server is listening on localhost:15432
#
# Exit code: 0 if every non-skipped module passed, non-zero otherwise.
# Final summary line: "N passed, K failed, M skipped".
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLONE_DIR="${HERE}/asyncpg"
VENV_DIR="${HERE}/.venv"
LOG="${HERE}/last-run.log"

if [[ ! -d "${CLONE_DIR}" ]] || [[ ! -d "${VENV_DIR}" ]]; then
  echo "ERROR: setup is incomplete. Run ./setup.sh first." >&2
  exit 2
fi

# Server reachable?
if ! (exec 3<>/dev/tcp/localhost/15432) 2>/dev/null; then
  echo "ERROR: cannot connect to localhost:15432 - is the pgwire server running?" >&2
  exit 2
fi
exec 3<&-; exec 3>&-

# shellcheck disable=SC1091
source "${VENV_DIR}/bin/activate"

# Env vars: asyncpg's testbase picks up PGHOST to mean "use the already-running
# cluster, don't try to spawn initdb".
export PGHOST=127.0.0.1
export PGPORT=15432
export PGUSER=datahike
export PGDATABASE=datahike
export PGPASSWORD=datahike
export PGSSLMODE=disable

# ---- focus list -------------------------------------------------------------
#
# Files under asyncpg/tests/ that exercise the extended-query protocol and
# core client behavior. See expected-skips.md for rationale on every entry
# here AND every entry deliberately absent.
MODULES=(
  "tests/test_connect.py"
  "tests/test_prepare.py"
  "tests/test_cache_invalidation.py"
  "tests/test_cursor.py"
  "tests/test_execute.py"
  "tests/test_exceptions.py"
  "tests/test_introspection.py"
  "tests/test_record.py"
  "tests/test_transaction.py"
  "tests/test_types.py"
  "tests/test_codecs.py"
)

cd "${CLONE_DIR}"

echo "[run] python -m pytest -v ${MODULES[*]}"
echo "[run] full output -> ${LOG}"

# Use pytest-style output (asyncpg uses unittest but pytest runs it fine).
# `-p no:cacheprovider` avoids creating .pytest_cache inside the upstream tree.
# tee so CircleCI sees progress (a plain redirect starves its
# no_output_timeout during the 10m+ run).
#
# --timeout: a per-test wall-clock cap so a server-side hang (e.g. the
# order-dependent test_cursor_iterable_02 stall) aborts that one test
# instead of blocking the whole job until CI's no_output_timeout kills it.
# --timeout-method=signal interrupts the asyncio event loop on the main
# thread (the default `thread` method can't unstick an asyncio wait).
# The hung test is reported as a failure; the suite still completes.
set -o pipefail
python -m pytest -v --tb=short -p no:cacheprovider \
  --timeout=60 --timeout-method=signal \
  "${MODULES[@]}" 2>&1 | tee "${LOG}"
RC=${PIPESTATUS[0]}

# --- summarize ---------------------------------------------------------------
#
# pytest's final line is of the form:
#   === 123 passed, 4 failed, 5 skipped, 1 error in 12.34s ===
LAST_SUMMARY="$(grep -E '^=+ .* (passed|failed|error|skipped).* =+$' "${LOG}" | tail -1)"

P=0; F=0; S=0; E=0
if [[ -n "${LAST_SUMMARY}" ]]; then
  P=$(  sed -n 's/.*[^0-9]\([0-9]*\) passed.*/\1/p'  <<<"${LAST_SUMMARY}" | head -1)
  F=$(  sed -n 's/.*[^0-9]\([0-9]*\) failed.*/\1/p'  <<<"${LAST_SUMMARY}" | head -1)
  S=$(  sed -n 's/.*[^0-9]\([0-9]*\) skipped.*/\1/p' <<<"${LAST_SUMMARY}" | head -1)
  E=$(  sed -n 's/.*[^0-9]\([0-9]*\) error.*/\1/p'   <<<"${LAST_SUMMARY}" | head -1)
fi
P=${P:-0}; F=${F:-0}; S=${S:-0}; E=${E:-0}
FAIL_TOTAL=$(( F + E ))

echo
echo "SUMMARY: ${P} passed, ${FAIL_TOTAL} failed, ${S} skipped   (pytest rc=${RC})"

if [[ ${RC} -ne 0 ]] || [[ ${FAIL_TOTAL} -gt 0 ]]; then
  echo
  echo "--- last 80 lines of ${LOG} ---"
  tail -n 80 "${LOG}" || true
  exit 1
fi
exit 0
