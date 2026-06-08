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

# Run each module as its OWN pytest invocation wrapped in a hard wall-clock
# `timeout`. A single combined run can hang indefinitely: a server-side
# protocol desync (e.g. the order-dependent test_cursor_iterable_02 stall)
# leaves asyncpg blocked inside its C-extension socket read, which neither
# pytest-timeout (signal can't interrupt the C select) nor a soft cap can
# unstick — only an external SIGKILL. Per-module + `timeout` means a hung
# module is killed and counted, and the remaining modules still run, so the
# suite always completes and reports. Per-module invocation also gives each
# module a clean interpreter (less cross-module state bleed).
PER_MODULE_TIMEOUT="${ASYNCPG_MODULE_TIMEOUT:-120}"
P=0; F=0; S=0; E=0; TIMED_OUT=()
set +e
: > "${LOG}"
for m in "${MODULES[@]}"; do
  echo "[run] ${m} (timeout ${PER_MODULE_TIMEOUT}s)"
  echo "==================== ${m} ====================" >> "${LOG}"
  timeout --signal=KILL "${PER_MODULE_TIMEOUT}" \
    python -m pytest -v --tb=short -p no:cacheprovider "${m}" >> "${LOG}" 2>&1
  rc=$?
  if [[ ${rc} -eq 137 ]]; then
    # SIGKILL from `timeout` — the module hung.
    echo "[run] TIMEOUT ${m}" | tee -a "${LOG}"
    TIMED_OUT+=("${m}")
    F=$(( F + 1 ))   # count a hung module as one failure
    continue
  fi
  # Parse this module's per-run summary line.
  ms="$(grep -E '^=+ .* (passed|failed|error|skipped).* =+$' "${LOG}" | tail -1)"
  if [[ -n "${ms}" ]]; then
    mp=$(sed -n 's/.*[^0-9]\([0-9]*\) passed.*/\1/p'  <<<"${ms}" | head -1)
    mf=$(sed -n 's/.*[^0-9]\([0-9]*\) failed.*/\1/p'  <<<"${ms}" | head -1)
    msk=$(sed -n 's/.*[^0-9]\([0-9]*\) skipped.*/\1/p' <<<"${ms}" | head -1)
    me=$(sed -n 's/.*[^0-9]\([0-9]*\) error.*/\1/p'   <<<"${ms}" | head -1)
    P=$(( P + ${mp:-0} )); F=$(( F + ${mf:-0} )); S=$(( S + ${msk:-0} )); E=$(( E + ${me:-0} ))
  fi
done
set -e
FAIL_TOTAL=$(( F + E ))

echo
echo "SUMMARY: ${P} passed, ${FAIL_TOTAL} failed, ${S} skipped"
if [[ ${#TIMED_OUT[@]} -gt 0 ]]; then
  echo "Timed-out modules (hung, SIGKILLed): ${TIMED_OUT[*]}"
fi

if [[ ${FAIL_TOTAL} -gt 0 ]]; then
  echo
  echo "Full output in ${LOG}"
  exit 1
fi
exit 0
