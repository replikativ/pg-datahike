#!/usr/bin/env bash
# Boot pg-datahike on :15432, point Odoo at it, run --init=$INIT_MODULE
# (default: base), optionally with --test-tags=$TEST_TAGS, parse the log,
# assert PASS_FLOOR.
#
# Env vars:
#   INIT_MODULE  — module(s) to init. Default `base`. Comma-separated for
#                  multi-module loads, e.g. `base,account`.
#   TEST_TAGS    — value for --test-tags. Default `:TestORM`. Set empty
#                  ("") to run --init only and skip the test-runner phase
#                  entirely (PASS_FLOOR enforcement is then skipped too).
#   PGWIRE_PORT  — port pgwire binds to. Default 15432.
#   PASS_FLOOR   — minimum PASS_COUNT (only enforced when TEST_TAGS != "").
#                  Default 9.
#
# Exit 0 on success; non-zero on:
#   - pgwire failed to boot
#   - Odoo failed to init the requested module(s)
#   - (only when TEST_TAGS is non-empty) fewer than $PASS_FLOOR cases passed
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${HERE}/../../.." && pwd)"
ODOO_ROOT="${ODOO_ROOT:-/home/christian-weilbach/Development/odoo}"
ODOO_VENV="${ODOO_VENV:-${ODOO_ROOT}/.venv}"
PGWIRE_PORT="${PGWIRE_PORT:-15432}"
INIT_MODULE="${INIT_MODULE:-base}"
TEST_TAGS="${TEST_TAGS-:TestORM}"   # default ":TestORM"; empty means "no tests"
PASS_FLOOR="${PASS_FLOOR:-9}"

LOG="${HERE}/last-run.log"
DATA_DIR="${HERE}/data"
CONF="${HERE}/odoo.conf"

need() { command -v "$1" >/dev/null 2>&1 || { echo "ERROR: missing tool: $1" >&2; exit 2; }; }
need clojure

if [[ ! -d "${ODOO_VENV}" ]]; then
  echo "ERROR: Odoo venv at ${ODOO_VENV} not found. Run ./setup.sh first." >&2
  exit 2
fi

# ---- cleanup state from prior runs ----------------------------------------
rm -rf "${DATA_DIR}"
mkdir -p "${DATA_DIR}"
: > "${LOG}"

# ---- write a fresh odoo.conf each run -------------------------------------
# Points Odoo at pg-datahike on :15432. `data_dir` is per-run so we
# don't leak filestores across runs.
cat > "${CONF}" <<EOF
[options]
addons_path = ${ODOO_ROOT}/addons,${ODOO_ROOT}/odoo/addons
data_dir = ${DATA_DIR}
db_host = localhost
db_port = ${PGWIRE_PORT}
db_user = datahike
db_password = datahike
db_name = datahike
db_maxconn = 8
log_level = warn
log_handler = :WARNING,odoo.modules:INFO,odoo.tests:INFO
without_demo = True
proxy_mode = False
EOF

# ---- boot pgwire (unfixtured) ---------------------------------------------
# Use start_pgwire.clj — Odoo's --init=base creates its own ~600 tables,
# we don't pre-seed.
echo "[run] starting pgwire on :${PGWIRE_PORT}"
pushd "${REPO_ROOT}" >/dev/null
clojure -M:server >>"${LOG}" 2>&1 &
PGWIRE_PID=$!
popd >/dev/null

cleanup() {
  local rc=$?
  echo "[run] cleanup (rc=${rc})"
  kill "${PGWIRE_PID}" 2>/dev/null || true
  sleep 1
  kill -9 "${PGWIRE_PID}" 2>/dev/null || true
  return $rc
}
trap cleanup EXIT

# Wait for pgwire (cold start: ~30 s on a slow disk).
echo "[run] waiting for pgwire on :${PGWIRE_PORT} (up to 90s)"
for _ in $(seq 1 90); do
  if (exec 3<>/dev/tcp/localhost/"${PGWIRE_PORT}") 2>/dev/null; then
    exec 3<&-; exec 3>&-
    echo "[run] pgwire up"
    break
  fi
  sleep 1
  if ! kill -0 "${PGWIRE_PID}" 2>/dev/null; then
    echo "ERROR: pgwire exited before port came up. Tail of ${LOG}:" >&2
    tail -40 "${LOG}" >&2
    exit 1
  fi
done

# ---- run Odoo init [+ tests] ----------------------------------------------
# `--stop-after-init` is the test-runner mode: run --init=<modules>,
# (optionally) run --test-tags=<filter>, then exit. Odoo writes per-test
# PASS/FAIL to the log under the `odoo.tests` logger.
ODOO_ARGS=(
  -c "${CONF}"
  --init="${INIT_MODULE}"
  --stop-after-init
)
if [[ -n "${TEST_TAGS}" ]]; then
  ODOO_ARGS+=(--test-tags="${TEST_TAGS}")
  echo "[run] running odoo --init=${INIT_MODULE} --test-tags=${TEST_TAGS}"
else
  echo "[run] running odoo --init=${INIT_MODULE} (no tests; init-only)"
fi
# shellcheck disable=SC1091
source "${ODOO_VENV}/bin/activate"
PYTHONPATH="${ODOO_ROOT}:${PYTHONPATH:-}" \
  python "${ODOO_ROOT}/odoo-bin" \
    "${ODOO_ARGS[@]}" \
    >>"${LOG}" 2>&1 || true   # don't trip set -e — we parse the log
ODOO_RC=$?

# ---- parse the log --------------------------------------------------------
# Init-only mode: skip the test parser entirely. Success criterion is
# Odoo exited 0 and the log shows the "Modules loaded." marker that
# odoo.modules.loading emits at the end of a successful registry build.
if [[ -z "${TEST_TAGS}" ]]; then
  echo
  echo "[run] odoo exit code: ${ODOO_RC}"
  if [[ "${ODOO_RC}" -ne 0 ]]; then
    echo
    echo "ERROR: odoo exited ${ODOO_RC} during --init=${INIT_MODULE}." >&2
    echo "Tail of ${LOG}:" >&2
    tail -120 "${LOG}" >&2
    exit 1
  fi
  if ! grep -qE 'odoo\.modules\.loading.*Modules loaded\.' "${LOG}"; then
    echo
    echo "ERROR: 'Modules loaded.' not found in ${LOG} — init incomplete." >&2
    echo "Tail of ${LOG}:" >&2
    tail -120 "${LOG}" >&2
    exit 1
  fi
  echo "[run] PASS — --init=${INIT_MODULE} completed (Modules loaded.)"
  exit 0
fi

# Test mode. Odoo 18+ emits a single result line:
#   `odoo.tests.result: <F> failed, <E> error(s) of <N> tests when loading database`
# Older Odoo (≤17) emitted `Ran N tests in T s` + per-test PASS/FAIL.
# Try the modern form first; fall back to the legacy one if missing.
RESULT_LINE="$(grep -E 'odoo\.tests\.result.*[0-9]+ failed.*[0-9]+ error.*of [0-9]+ tests' \
                    "${LOG}" 2>/dev/null | tail -1 || true)"

if [[ -n "${RESULT_LINE}" ]]; then
  TESTS_RAN="$(echo "${RESULT_LINE}" | grep -oE 'of [0-9]+ tests' | grep -oE '[0-9]+' | head -1)"
  FAILURES="$(echo "${RESULT_LINE}" | grep -oE '[0-9]+ failed'   | grep -oE '[0-9]+' | head -1)"
  ERRORS="$(echo   "${RESULT_LINE}" | grep -oE '[0-9]+ error'    | grep -oE '[0-9]+' | head -1)"
  PASS_COUNT=$(( ${TESTS_RAN:-0} - ${FAILURES:-0} - ${ERRORS:-0} ))
  SUMMARY="${RESULT_LINE}"
else
  # Legacy parser. Grep with `|| true` so a no-match (exit 1) doesn't
  # leak a stray "0\n0" into FAILURES — that would break the
  # arithmetic context further down.
  SUMMARY="$(grep -E 'odoo\.tests.*Ran [0-9]+ tests' "${LOG}" 2>/dev/null | tail -1 || true)"
  TESTS_RAN="$(echo "${SUMMARY}" | grep -oE 'Ran [0-9]+' | grep -oE '[0-9]+' | head -1)"
  FAILURES="$(grep -cE 'FAIL: |ERROR: ' "${LOG}" 2>/dev/null || true)"
  PASS_COUNT=$(( ${TESTS_RAN:-0} - ${FAILURES:-0} ))
fi

echo
echo "[run] odoo exit code: ${ODOO_RC}"
echo "[run] tests ran: ${TESTS_RAN:-?}"
echo "[run] passed: ${PASS_COUNT}"
echo "[run] failed: ${FAILURES}"
echo "[run] summary: ${SUMMARY:-<no summary line>}"

if [[ "${PASS_COUNT}" -lt "${PASS_FLOOR}" ]]; then
  echo
  echo "ERROR: PASS_COUNT=${PASS_COUNT} < PASS_FLOOR=${PASS_FLOOR}." >&2
  echo "Tail of ${LOG}:" >&2
  tail -80 "${LOG}" >&2
  exit 1
fi

echo "[run] PASS — ${PASS_COUNT} ≥ ${PASS_FLOOR}"
