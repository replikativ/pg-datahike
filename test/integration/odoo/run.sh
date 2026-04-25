#!/usr/bin/env bash
# Boot pg-datahike on :15432, point Odoo at it, run --init=base
# --test-tags=:TestORM, parse the log, assert PASS_FLOOR.
#
# Exit 0 on success; non-zero on:
#   - pgwire failed to boot
#   - Odoo failed to init the base module
#   - fewer than $PASS_FLOOR (default 9) TestORM cases passed
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${HERE}/../../.." && pwd)"
ODOO_ROOT="${ODOO_ROOT:-/home/christian-weilbach/Development/odoo}"
ODOO_VENV="${ODOO_VENV:-${ODOO_ROOT}/.venv}"
PGWIRE_PORT="${PGWIRE_PORT:-15432}"
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

# ---- run Odoo init + TestORM ----------------------------------------------
# `--stop-after-init` is the test-runner mode: run --init=<modules>,
# run --test-tags=<filter>, then exit. Odoo writes per-test PASS/FAIL
# to the log under the `odoo.tests` logger.
echo "[run] running odoo --init=base --test-tags=:TestORM"
# shellcheck disable=SC1091
source "${ODOO_VENV}/bin/activate"
PYTHONPATH="${ODOO_ROOT}:${PYTHONPATH:-}" \
  python "${ODOO_ROOT}/odoo-bin" \
    -c "${CONF}" \
    --init=base \
    --test-tags=:TestORM \
    --stop-after-init \
    >>"${LOG}" 2>&1 || true   # don't trip set -e — we parse the log
ODOO_RC=$?

# ---- parse the log --------------------------------------------------------
# Odoo writes "Module base loaded" then "PASS: <test_name>" or
# "FAIL: <test_name>" per test. Count both.
PASSED=$(grep -cE '^.*odoo\.tests.*\b(test_|TestORM)' "${LOG}" 2>/dev/null || echo 0)
RAN=$(grep -cE 'odoo\.tests\.runner.*\bran\b' "${LOG}" 2>/dev/null | head -1)
SUMMARY="$(grep -E 'odoo\.tests.*Ran [0-9]+ tests' "${LOG}" | tail -1)"

# More reliable: look for the explicit "OK"/"FAILED" + test count.
TESTS_RAN=$(echo "${SUMMARY}" | grep -oE 'Ran [0-9]+' | grep -oE '[0-9]+' | head -1)
FAILURES=$(grep -cE 'FAIL: |ERROR: ' "${LOG}" 2>/dev/null || echo 0)
PASS_COUNT=$(( ${TESTS_RAN:-0} - FAILURES ))

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
