#!/usr/bin/env bash
# Run a curated subset of the node-postgres integration tests.
#
# The upstream harness runs each test file as a separate `node file.js` call
# and relies on exit codes: 0 = pass, anything else = fail. We replicate that
# contract here, iterating over a focus list.
#
# Requirements:
#   - setup.sh has been run
#   - Datahike pgwire server is listening on localhost:15432
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLONE_DIR="${HERE}/node-postgres"
LOG="${HERE}/last-run.log"

if [[ ! -d "${CLONE_DIR}" ]]; then
  echo "ERROR: node-postgres/ is missing. Run ./setup.sh first." >&2
  exit 2
fi

# Server reachable?
if ! (exec 3<>/dev/tcp/localhost/15432) 2>/dev/null; then
  echo "ERROR: cannot connect to localhost:15432 - is the pgwire server running?" >&2
  exit 2
fi
exec 3<&-; exec 3>&-

# Env vars consumed by packages/pg/test/test-helper.js.
export PGHOST=127.0.0.1
export PGPORT=15432
export PGUSER=datahike
export PGDATABASE=datahike
export PGPASSWORD=datahike
export PGTESTNOSSL=1       # upstream flag: skip SSL tests
# Leave PG_CLIENT_ENCODING / PGSSLMODE unset: the default path is already
# trusted-local, no-TLS test harness.

PG_DIR="${CLONE_DIR}/packages/pg"
if [[ ! -d "${PG_DIR}/test/integration/client" ]]; then
  echo "ERROR: upstream layout changed; expected ${PG_DIR}/test/integration/client" >&2
  exit 2
fi

# --- focus list --------------------------------------------------------------
#
# Paths relative to ${PG_DIR}. Each entry is one `-tests.js` file that is
# executed as `node <file>`. The upstream harness in the Makefile does exactly
# this via `xargs -n 1 -I file node file`.
# Files that must pass — a regression here fails the job.
FILES=(
  # --- core protocol surface ---------------------------------------------
  "test/integration/client/big-simple-query-tests.js"
  "test/integration/client/simple-query-tests.js"
  "test/integration/client/empty-query-tests.js"
  "test/integration/client/prepared-statement-tests.js"
  "test/integration/client/multiple-results-tests.js"
  "test/integration/client/no-data-tests.js"
  "test/integration/client/no-row-result-tests.js"
  "test/integration/client/row-description-on-results-tests.js"
  "test/integration/client/result-metadata-tests.js"
  "test/integration/client/results-as-array-tests.js"
  "test/integration/client/query-column-names-tests.js"

  # --- API shape (promise / callback) ------------------------------------
  "test/integration/client/promise-api-tests.js"
  "test/integration/client/query-as-promise-tests.js"
  "test/integration/client/api-tests.js"

  # --- transactions ------------------------------------------------------
  "test/integration/client/transaction-tests.js"

  # --- type coercion -----------------------------------------------------
  "test/integration/client/json-type-parsing-tests.js"
)

# Known-gap files (xfail): each fails on a specific, documented missing
# feature — NOT a regression. See expected-skips.md for the per-file
# rationale and tracking. They still run every time so we notice when a
# fix flips one green (reported as XPASS — promote it into FILES above).
# A failure here counts as "skipped", not "failed", so the job stays green.
XFAIL_FILES=(
  "test/integration/client/error-handling-tests.js"
  # backslash / exotic quoted-identifier escaping (JSqlParser identifier rules).
  "test/integration/client/field-name-escape-tests.js"
  # query cancellation: pg_cancel_backend / pg_terminate_backend over
  # pg_stat_activity not implemented.
  "test/integration/client/query-error-handling-tests.js"
  "test/integration/client/query-error-handling-prepared-statement-tests.js"
  # Extreme PostgreSQL/ECMAScript date range; the NULL-comparison case passes.
  "test/integration/client/type-coercion-tests.js"
  # COUNT(*) over an empty table + '{1,2,3}'::bigint[] array-literal cast.
  "test/integration/client/parse-int-8-tests.js"
)

cd "${PG_DIR}"
: > "${LOG}"

# --- seed test dataset -------------------------------------------------------
#
# Upstream's `make test-integration` depends on `test-connection`, which runs
# `node script/create-test-tables.js` to (re)create the `person` table seeded
# with 26 rows (Aaron..Zanzabar). Several test files (simple-query,
# big-simple-query, prepared-statement, parse-int-8, query-error-handling)
# SELECT from `person` and assert on those exact 26 rows. Without this step
# they fail with "0 == 26". The script reads the PG* env vars exported above
# and is idempotent (DROP TABLE IF EXISTS person; CREATE TABLE …; INSERT …).
echo "[run] seeding test dataset (person)"
echo "---- seed: script/create-test-tables.js ----" >> "${LOG}"
if ! node "${PG_DIR}/script/create-test-tables.js" >> "${LOG}" 2>&1; then
  echo "ERROR: create-test-tables.js failed; see ${LOG}" >&2
  exit 2
fi

PASS=0
FAIL=0
FAILED_FILES=()

for f in "${FILES[@]}"; do
  if [[ ! -f "${f}" ]]; then
    echo "[run] MISSING ${f}" | tee -a "${LOG}"
    FAIL=$(( FAIL + 1 ))
    FAILED_FILES+=("${f} (missing)")
    continue
  fi
  echo "---- ${f} ----" >> "${LOG}"
  if node "${f}" >> "${LOG}" 2>&1; then
    printf '[run] PASS %s\n' "${f}"
    PASS=$(( PASS + 1 ))
  else
    RC=$?
    printf '[run] FAIL %s (rc=%d)\n' "${f}" "${RC}"
    FAIL=$(( FAIL + 1 ))
    FAILED_FILES+=("${f} (rc=${RC})")
  fi
done

# Known-gap (xfail) files: run them, but an expected failure counts as a
# skip rather than failing the job. An unexpected PASS is reported as XPASS
# so we promote it into FILES.
SKIP=0
XPASS_FILES=()
for f in "${XFAIL_FILES[@]}"; do
  if [[ ! -f "${f}" ]]; then
    echo "[run] MISSING (xfail) ${f}" | tee -a "${LOG}"
    SKIP=$(( SKIP + 1 ))
    continue
  fi
  echo "---- (xfail) ${f} ----" >> "${LOG}"
  if node "${f}" >> "${LOG}" 2>&1; then
    printf '[run] XPASS %s  <-- now passes; promote into FILES\n' "${f}"
    XPASS_FILES+=("${f}")
    PASS=$(( PASS + 1 ))
  else
    printf '[run] xfail %s (expected; see expected-skips.md)\n' "${f}"
    SKIP=$(( SKIP + 1 ))
  fi
done

echo
echo "SUMMARY: ${PASS} passed, ${FAIL} failed, ${SKIP} skipped"
if [[ "${#XPASS_FILES[@]}" -gt 0 ]]; then
  echo
  echo "Unexpectedly passing (XPASS) — promote into the must-pass list:"
  for f in "${XPASS_FILES[@]}"; do echo "  + ${f}"; done
fi
if [[ "${FAIL}" -gt 0 ]]; then
  echo
  echo "Failing files (regressions in the must-pass list):"
  for f in "${FAILED_FILES[@]}"; do echo "  - ${f}"; done
  echo
  echo "Full output in ${LOG}"
  exit 1
fi
exit 0
