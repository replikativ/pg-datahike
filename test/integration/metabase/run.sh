#!/usr/bin/env bash
# Drive a real Metabase instance against pg-datahike and assert that
# `Sync database` completes successfully — Metabase's first-class
# probe of every catalog / pg_get_*def / format_type / current_setting
# code path we just implemented.
#
# Flow:
#   1. Boot the seeded Datahike pgwire server (test/integration/metabase/seed.clj).
#   2. Launch metabase.jar with a fresh H2 metadata DB under ./data/.
#   3. Wait for /api/health → 200.
#   4. POST /api/setup → create admin user.
#   5. POST /api/database → add pg-datahike as a Postgres datasource.
#   6. Poll /api/database/<id> until :is_full_sync becomes false (sync done).
#   7. GET /api/database/<id>/metadata → assert tables / fields appear.
#   8. Tear down both processes.
#
# Exit code: 0 if every assertion passes, non-zero otherwise.
# Logs from Metabase + pgwire end up in ./last-run.log.
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${HERE}/../../.." && pwd)"
JAR="${HERE}/metabase.jar"
LOG="${HERE}/last-run.log"
DATA_DIR="${HERE}/data"
MB_PORT="${MB_PORT:-3000}"
PGWIRE_PORT="${PGWIRE_PORT:-15432}"

# ---- prereqs ----------------------------------------------------------------

have() { command -v "$1" >/dev/null 2>&1; }
need() { have "$1" || { echo "ERROR: missing tool: $1" >&2; exit 2; }; }
need curl
need jq

if [[ ! -f "${JAR}" ]]; then
  echo "ERROR: metabase.jar not found at ${JAR}. Run ./setup.sh first." >&2
  exit 2
fi

# Metabase 0.50+ requires Java 21. Match setup.sh's JDK detection.
JAVA_HOME_OVERRIDE="${METABASE_JDK_HOME:-}"
if [[ -z "${JAVA_HOME_OVERRIDE}" ]]; then
  for candidate in \
    "/usr/lib/jvm/java-21-openjdk-amd64" \
    "/usr/lib/jvm/temurin-21-jdk-amd64" \
    "/Library/Java/JavaVirtualMachines/temurin-21.jdk/Contents/Home"; do
    if [[ -x "${candidate}/bin/java" ]]; then
      JAVA_HOME_OVERRIDE="${candidate}"
      break
    fi
  done
fi
if [[ -n "${JAVA_HOME_OVERRIDE}" ]]; then
  export JAVA_HOME="${JAVA_HOME_OVERRIDE}"
  export PATH="${JAVA_HOME}/bin:${PATH}"
fi

# ---- cleanup state from prior runs ------------------------------------------

rm -rf "${DATA_DIR}"
mkdir -p "${DATA_DIR}"
: > "${LOG}"

# ---- start pgwire (seeded fixture) -----------------------------------------

echo "[run] starting seeded pgwire on :${PGWIRE_PORT}"
pushd "${REPO_ROOT}" >/dev/null
clojure -M:server "${HERE}/seed.clj" >>"${LOG}" 2>&1 &
PGWIRE_PID=$!
popd >/dev/null

cleanup() {
  local rc=$?
  echo "[run] cleanup (rc=${rc})"
  kill "${PGWIRE_PID}"  2>/dev/null || true
  kill "${MB_PID:-}"    2>/dev/null || true
  # Give them a beat to release ports / flush logs.
  sleep 1
  kill -9 "${PGWIRE_PID}" 2>/dev/null || true
  kill -9 "${MB_PID:-}"   2>/dev/null || true
  return $rc
}
trap cleanup EXIT

# Wait for pgwire — at most 60s (datahike + tools.deps cold start).
echo "[run] waiting for pgwire on :${PGWIRE_PORT}"
for i in $(seq 1 60); do
  if (exec 3<>/dev/tcp/localhost/"${PGWIRE_PORT}") 2>/dev/null; then
    exec 3<&-; exec 3>&-
    echo "[run] pgwire up"
    break
  fi
  sleep 1
  if ! kill -0 "${PGWIRE_PID}" 2>/dev/null; then
    echo "ERROR: pgwire process exited before port came up. See ${LOG}." >&2
    exit 1
  fi
done

# ---- start Metabase ---------------------------------------------------------

echo "[run] starting Metabase on :${MB_PORT} (this is slow — ~60s on first boot)"
(
  cd "${DATA_DIR}"
  MB_DB_TYPE=h2 \
  MB_DB_FILE="${DATA_DIR}/metabase" \
  MB_JETTY_PORT="${MB_PORT}" \
  MB_LOG_LEVEL=warn \
  java -jar "${JAR}" >>"${LOG}" 2>&1
) &
MB_PID=$!

# Wait for /api/health → 200 — Metabase JIT compiles a lot on first boot.
echo "[run] waiting for /api/health (up to 240s)"
HEALTHY=0
for i in $(seq 1 240); do
  if curl -fsS "http://localhost:${MB_PORT}/api/health" >/dev/null 2>&1; then
    HEALTHY=1
    break
  fi
  sleep 1
  if ! kill -0 "${MB_PID}" 2>/dev/null; then
    echo "ERROR: metabase exited during boot. See ${LOG}." >&2
    exit 1
  fi
done
if [[ "${HEALTHY}" -ne 1 ]]; then
  echo "ERROR: metabase did not become healthy within 240s. Tail of ${LOG}:" >&2
  tail -30 "${LOG}" >&2
  exit 1
fi
echo "[run] metabase healthy"

# ---- setup token + admin user ----------------------------------------------

SETUP_TOKEN="$(curl -fsS "http://localhost:${MB_PORT}/api/session/properties" \
  | jq -r '."setup-token"')"
if [[ -z "${SETUP_TOKEN}" || "${SETUP_TOKEN}" == "null" ]]; then
  echo "ERROR: could not read setup-token from /api/session/properties." >&2
  exit 1
fi
echo "[run] setup-token: ${SETUP_TOKEN:0:8}…"

# Create the first admin user. Metabase requires this before allowing
# any other API call. The "database" block is the metadata DB; we
# leave that as the default H2 by passing `null` for engine-specific
# fields, then add our pg-datahike datasource separately below.
SETUP_BODY=$(jq -n --arg tok "${SETUP_TOKEN}" '
{ token: $tok,
  prefs: { site_name: "datahike-test", site_locale: "en",
           allow_tracking: false },
  user:  { first_name: "Test", last_name: "Admin",
           email: "test@datahike.local",
           password: "datahike-test-1!",
           password_confirm: "datahike-test-1!",
           site_name: "datahike-test" } }')
SESSION_TOKEN="$(curl -fsS -X POST \
  -H "Content-Type: application/json" \
  --data "${SETUP_BODY}" \
  "http://localhost:${MB_PORT}/api/setup" | jq -r '.id')"
if [[ -z "${SESSION_TOKEN}" || "${SESSION_TOKEN}" == "null" ]]; then
  echo "ERROR: /api/setup did not return a session id." >&2
  exit 1
fi
echo "[run] admin user created, session: ${SESSION_TOKEN:0:8}…"

mb() {
  curl -fsS -H "X-Metabase-Session: ${SESSION_TOKEN}" \
    -H "Content-Type: application/json" "$@"
}

# ---- add pg-datahike as a Postgres datasource ------------------------------

DB_BODY=$(jq -n --arg port "${PGWIRE_PORT}" '
{ engine:    "postgres",
  name:      "pg-datahike",
  details:   { host: "localhost", port: ($port|tonumber),
               db: "datahike", user: "datahike", password: "datahike",
               ssl: false, tunnel-enabled: false,
               advanced-options: false } }')
DB_ID="$(mb -X POST --data "${DB_BODY}" \
  "http://localhost:${MB_PORT}/api/database" | jq -r '.id')"
if [[ -z "${DB_ID}" || "${DB_ID}" == "null" ]]; then
  echo "ERROR: /api/database did not return an id." >&2
  exit 1
fi
echo "[run] datasource created: id=${DB_ID}"

# ---- wait for sync ---------------------------------------------------------

# Metabase kicks off an async sync immediately on database creation.
# initial_sync_status flips from "incomplete" to "complete" when done.
echo "[run] polling /api/database/${DB_ID} for initial_sync_status (up to 120s)"
SYNC_DONE=0
for i in $(seq 1 120); do
  STATUS="$(mb "http://localhost:${MB_PORT}/api/database/${DB_ID}" \
    | jq -r '.initial_sync_status')"
  if [[ "${STATUS}" == "complete" ]]; then
    SYNC_DONE=1
    echo "[run] sync complete (after ${i}s)"
    break
  fi
  sleep 1
done
if [[ "${SYNC_DONE}" -ne 1 ]]; then
  echo "ERROR: initial sync never completed. Last status: ${STATUS}" >&2
  echo "Tail of ${LOG}:" >&2
  tail -50 "${LOG}" >&2
  exit 1
fi

# ---- assert metadata shape -------------------------------------------------

META="$(mb "http://localhost:${MB_PORT}/api/database/${DB_ID}/metadata")"
NUM_TABLES=$(echo "${META}" | jq '.tables | length')
TABLE_NAMES=$(echo "${META}" | jq -r '.tables[].name' | sort | paste -sd, -)
NUM_FIELDS=$(echo "${META}" | jq '[.tables[].fields[]] | length')

echo "[run] tables seen by Metabase (${NUM_TABLES}): ${TABLE_NAMES}"
echo "[run] field count: ${NUM_FIELDS}"

FAIL=0
if [[ "${NUM_TABLES}" -lt 2 ]]; then
  echo "ERROR: expected >= 2 user tables (customer, order), got ${NUM_TABLES}" >&2
  FAIL=1
fi
if [[ "${NUM_FIELDS}" -lt 9 ]]; then
  echo "ERROR: expected >= 9 fields across tables, got ${NUM_FIELDS}" >&2
  FAIL=1
fi
if ! echo "${TABLE_NAMES}" | grep -q customer; then
  echo "ERROR: customer table missing from sync output" >&2
  FAIL=1
fi
if ! echo "${TABLE_NAMES}" | grep -q order; then
  echo "ERROR: order table missing from sync output" >&2
  FAIL=1
fi

if [[ "${FAIL}" -ne 0 ]]; then
  echo "Tail of ${LOG}:" >&2
  tail -60 "${LOG}" >&2
  exit 1
fi

# ---- bonus: round-trip a query through Metabase's query processor ---------
# This issues a native query via Metabase's API — Metabase translates
# it, sends it to pg-datahike, and parses the result. If this passes,
# the full ingest path is verified end-to-end.

QRY=$(jq -n --argjson dbid "${DB_ID}" '
{ database: $dbid, type: "native",
  native:   { query: "SELECT count(*) FROM customer" } }')
QRES="$(mb -X POST --data "${QRY}" \
  "http://localhost:${MB_PORT}/api/dataset")"
ROW=$(echo "${QRES}" | jq -r '.data.rows[0][0]')
if [[ "${ROW}" == "3" ]]; then
  echo "[run] native query: SELECT count(*) FROM customer → 3 ✓"
else
  echo "ERROR: native query returned '${ROW}', expected 3" >&2
  echo "Full response:" >&2
  echo "${QRES}" | jq '.' >&2
  exit 1
fi

echo "[run] PASS — Metabase synced ${NUM_TABLES} tables / ${NUM_FIELDS} fields and ran a native query"
