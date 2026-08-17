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

# Deselect the whole async-iterable-cursor class. It exercises server-side
# cursor STREAMING (asyncpg fetches rows in portal batches expecting
# PortalSuspended) — a known structural gap (portal streaming / A3, see
# doc/design-alignment.md). Against our non-streaming server these tests are
# unreliable: test_cursor_iterable_02 hangs outright, and _06 flips
# pass/fail by execution order. Flaky/hanging tests can't be expressed as
# stable "expected failures", so the class is removed from collection. The
# deterministic non-streaming TestCursor tests still run (02/04 are listed
# in expected-failures.txt). The per-module SIGKILL timeout above stays a
# backstop: any OTHER module that hangs is then an unexpected regression.
DESELECT="tests/test_cursor.py::TestIterableCursor"

P=0; F=0; S=0; E=0; TIMED_OUT=()
set +e
: > "${LOG}"
for m in "${MODULES[@]}"; do
  echo "[run] ${m} (timeout ${PER_MODULE_TIMEOUT}s)"
  echo "==================== ${m} ====================" >> "${LOG}"
  timeout --signal=KILL "${PER_MODULE_TIMEOUT}" \
    python -m pytest -v --tb=short -p no:cacheprovider \
    --deselect "${DESELECT}" "${m}" >> "${LOG}" 2>&1
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

# --- regression gate -------------------------------------------------------
# The suite is not green (the SUT doesn't yet cover everything asyncpg
# probes), so a raw failure count can't gate the job. Instead diff the live
# FAILED/ERROR set against a checked-in manifest of known gaps: the job is
# green as long as failures stay within that set. A failure NOT listed is a
# regression; a listed test that now passes is a (non-fatal) nudge to prune.
MANIFEST="${HERE}/expected-failures.txt"
ACTUAL_TMP="$(mktemp)"; EXPECTED_TMP="$(mktemp)"
trap 'rm -f "${ACTUAL_TMP}" "${EXPECTED_TMP}" "${RAN_TMP:-}"' EXIT

# Live failures: pytest -v prints "tests/x.py::Class::test FAILED|ERROR" (no
# percentage suffix when stdout is not a TTY, as here under redirection).
grep -hoE '^tests/[^ ]+ (FAILED|ERROR)$' "${LOG}" \
  | sed -E 's/ (FAILED|ERROR)$//' | sort -u > "${ACTUAL_TMP}"
# Manifest test-IDs only (lines beginning `tests/` — skips comments/blanks).
# Test IDs carry no internal whitespace, so a plain line grep is exact.
grep -E '^tests/' "${MANIFEST}" | sort -u > "${EXPECTED_TMP}"

NEW="$(comm -23 "${ACTUAL_TMP}" "${EXPECTED_TMP}")"

# A manifest entry that is absent from the live FAILED set has two very
# different explanations, and collapsing them made the nudge unusable:
# the test now PASSES (prune it), or the test NEVER RAN (a coverage hole
# — the gate silently stopped checking it). Separate them by first
# collecting every test that produced ANY verdict this run.
RAN_TMP="$(mktemp)"
grep -hoE '^tests/[^ ]+ (PASSED|FAILED|ERROR|SKIPPED)$' "${LOG}" \
  | sed -E 's/ (PASSED|FAILED|ERROR|SKIPPED)$//' | sort -u > "${RAN_TMP}"

# In the manifest, ran, but not failing -> genuinely resolved.
RESOLVED="$(comm -13 "${ACTUAL_TMP}" "${EXPECTED_TMP}" | comm -12 - "${RAN_TMP}")"
# In the manifest and never ran -> not covered any more.
MISSING="$(comm -13 "${RAN_TMP}" "${EXPECTED_TMP}")"

rc=0
if [[ ${#TIMED_OUT[@]} -gt 0 ]]; then
  echo
  echo "REGRESSION: module(s) hung and were SIGKILLed. The only known hang"
  echo "(test_cursor_iterable_02) is deselected, so this is unexpected:"
  printf '  %s\n' "${TIMED_OUT[@]}"
  rc=1
fi
if [[ -n "${NEW}" ]]; then
  echo
  echo "REGRESSION: $(grep -c . <<<"${NEW}") test(s) failing that are NOT in"
  echo "expected-failures.txt — fix them or add them with a rationale:"
  sed 's/^/  /' <<<"${NEW}"
  rc=1
fi
if [[ -n "${RESOLVED}" ]]; then
  echo
  echo "NOTE: $(grep -c . <<<"${RESOLVED}") expected-failure(s) now PASS —"
  echo "prune expected-failures.txt to keep the gate honest:"
  sed 's/^/  /' <<<"${RESOLVED}"
fi
if [[ -n "${MISSING}" ]]; then
  echo
  echo "NOTE: $(grep -c . <<<"${MISSING}") expected-failure(s) DID NOT RUN. These"
  echo "are not fixed — the gate has simply stopped checking them (renamed,"
  echo "deselected, or the module died before reaching them):"
  sed 's/^/  /' <<<"${MISSING}"
fi

echo
echo "Full output in ${LOG}"
exit ${rc}
