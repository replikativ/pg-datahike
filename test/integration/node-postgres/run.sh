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
# cleartext, no-TLS.

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
FILES=(
  # --- core protocol surface ---------------------------------------------
  "test/integration/client/simple-query-tests.js"
  "test/integration/client/big-simple-query-tests.js"
  "test/integration/client/empty-query-tests.js"
  "test/integration/client/prepared-statement-tests.js"
  "test/integration/client/multiple-results-tests.js"
  "test/integration/client/no-data-tests.js"
  "test/integration/client/no-row-result-tests.js"
  "test/integration/client/row-description-on-results-tests.js"
  "test/integration/client/result-metadata-tests.js"
  "test/integration/client/results-as-array-tests.js"
  "test/integration/client/query-column-names-tests.js"
  "test/integration/client/field-name-escape-tests.js"

  # --- error / edge cases ------------------------------------------------
  "test/integration/client/error-handling-tests.js"
  "test/integration/client/query-error-handling-tests.js"
  "test/integration/client/query-error-handling-prepared-statement-tests.js"

  # --- API shape (promise / callback) ------------------------------------
  "test/integration/client/api-tests.js"
  "test/integration/client/promise-api-tests.js"
  "test/integration/client/query-as-promise-tests.js"

  # --- transactions ------------------------------------------------------
  "test/integration/client/transaction-tests.js"

  # --- basic type coercion -----------------------------------------------
  "test/integration/client/type-coercion-tests.js"
  "test/integration/client/parse-int-8-tests.js"
  "test/integration/client/json-type-parsing-tests.js"
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

# node-postgres has no "skipped" concept at the file level; a few tests inside
# each file may silently early-return, but we can't see those here.
SKIP=0

echo
echo "SUMMARY: ${PASS} passed, ${FAIL} failed, ${SKIP} skipped"
if [[ "${FAIL}" -gt 0 ]]; then
  echo
  echo "Failing files:"
  for f in "${FAILED_FILES[@]}"; do echo "  - ${f}"; done
  echo
  echo "Full output in ${LOG}"
  exit 1
fi
exit 0
