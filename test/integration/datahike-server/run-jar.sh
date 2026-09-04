#!/usr/bin/env bash
# Release soak for the Datahike Server standalone JAR with pg-datahike enabled.
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 path/to/datahike-http-server-VERSION-standalone.jar" >&2
  exit 2
fi

JAR="$1"
EXPECTED_PG_VERSION="${EXPECTED_PG_DATAHIKE_VERSION:-0.1.189}"
EXPECTED_DATAHIKE_VERSION="${EXPECTED_DATAHIKE_VERSION:-}"
PSQL="${PSQL:-psql}"

for command in curl java keytool sha256sum unzip "${PSQL}"; do
  command -v "${command}" >/dev/null || {
    echo "ERROR: required command not found: ${command}" >&2
    exit 2
  }
done
[[ -f "${JAR}" ]] || { echo "ERROR: JAR not found: ${JAR}" >&2; exit 2; }

embedded_pg_version="$(unzip -p "${JAR}" pg-datahike.version)"
embedded_datahike_version="$(unzip -p "${JAR}" DATAHIKE_VERSION)"
if [[ "${embedded_pg_version}" != "${EXPECTED_PG_VERSION}" ]]; then
  echo "ERROR: expected pg-datahike ${EXPECTED_PG_VERSION}, JAR contains ${embedded_pg_version}" >&2
  exit 1
fi
if [[ -n "${EXPECTED_DATAHIKE_VERSION}" &&
      "${embedded_datahike_version}" != "${EXPECTED_DATAHIKE_VERSION}" ]]; then
  echo "ERROR: expected Datahike ${EXPECTED_DATAHIKE_VERSION}, JAR contains ${embedded_datahike_version}" >&2
  exit 1
fi

free_port() {
  local candidate
  for candidate in $(seq "$1" "$2"); do
    if ! (exec 3<>"/dev/tcp/127.0.0.1/${candidate}") 2>/dev/null; then
      echo "${candidate}"
      return
    fi
  done
  echo "ERROR: no free port in range $1-$2" >&2
  exit 1
}

HTTP_PORT="${DATAHIKE_SOAK_HTTP_PORT:-$(free_port 18444 18543)}"
PG_PORT="${DATAHIKE_SOAK_PG_PORT:-$(free_port 15439 15538)}"
TMP_DIR="$(mktemp -d /tmp/pg-datahike-server-soak.XXXXXX)"
CONFIG="${TMP_DIR}/server.edn"
KEYSTORE="${TMP_DIR}/server.p12"
CA_CERT="${TMP_DIR}/server.crt"
LOG="${TMP_DIR}/server.log"
SERVER_PID=""
TOKEN="datahike-server-soak-http-token"
PG_PASSWORD="datahike-server-soak-pg-password"

cleanup() {
  if [[ -n "${SERVER_PID}" ]] && kill -0 "${SERVER_PID}" 2>/dev/null; then
    kill -TERM "${SERVER_PID}" 2>/dev/null || true
    wait "${SERVER_PID}" 2>/dev/null || true
  fi
  rm -rf "${TMP_DIR}"
}
trap cleanup EXIT

keytool -genkeypair -noprompt \
  -alias datahike-server \
  -storetype PKCS12 \
  -keystore "${KEYSTORE}" \
  -storepass changeit \
  -keypass changeit \
  -keyalg RSA \
  -keysize 2048 \
  -validity 2 \
  -dname "CN=localhost, OU=release-soak, O=Datahike, C=DE" \
  -ext "SAN=dns:localhost,ip:127.0.0.1" >/dev/null 2>&1
keytool -exportcert -rfc \
  -alias datahike-server \
  -keystore "${KEYSTORE}" \
  -storepass changeit \
  -file "${CA_CERT}" >/dev/null 2>&1

cat >"${CONFIG}" <<EOF
{:host "127.0.0.1"
 :port ${HTTP_PORT}
 :token "${TOKEN}"
 :shutdown-timeout-ms 5000
 :system-db {:store {:backend :file :path "${TMP_DIR}/system"}}
 :pg-listener
 {:host "127.0.0.1"
  :port ${PG_PORT}
  :users {"app" "${PG_PASSWORD}"}
  :require-tls? true
  :tls {:keystore "${KEYSTORE}"
        :keystore-password "changeit"}}}
EOF

dump_log_and_fail() {
  echo "ERROR: $1" >&2
  tail -120 "${LOG}" >&2 || true
  exit 1
}

start_server() {
  : >"${LOG}"
  java -jar "${JAR}" --config "${CONFIG}" >"${LOG}" 2>&1 &
  SERVER_PID=$!
  local attempt
  for attempt in $(seq 1 480); do
    kill -0 "${SERVER_PID}" 2>/dev/null || dump_log_and_fail "server exited during startup"
    if curl --silent --fail "http://127.0.0.1:${HTTP_PORT}/health/live" >/dev/null; then
      return
    fi
    sleep 0.25
  done
  dump_log_and_fail "server did not become live within 120 seconds"
}

stop_server() {
  kill -TERM "${SERVER_PID}"
  local attempt
  for attempt in $(seq 1 80); do
    if ! kill -0 "${SERVER_PID}" 2>/dev/null; then
      # A normal SIGTERM shutdown reports 128 + TERM from the launcher.
      wait "${SERVER_PID}" || true
      SERVER_PID=""
      return
    fi
    sleep 0.25
  done
  dump_log_and_fail "server did not stop gracefully within 20 seconds"
}

psql_tls() {
  PGPASSWORD="${PG_PASSWORD}" PGCONNECT_TIMEOUT=5 PGSSLMODE=verify-full \
    PGSSLROOTCERT="${CA_CERT}" \
    "${PSQL}" --no-psqlrc --host localhost --port "${PG_PORT}" \
    --username app --dbname soak "$@"
}

wait_for_pg() {
  local attempt
  for attempt in $(seq 1 120); do
    if psql_tls --tuples-only --no-align --command "SELECT 1" >/dev/null 2>&1; then
      return
    fi
    sleep 0.25
  done
  dump_log_and_fail "PostgreSQL listener did not accept TLS connections within 30 seconds"
}

start_server

create_response="${TMP_DIR}/create-response.txt"
create_status="$(curl --silent --show-error \
  --output "${create_response}" --write-out '%{http_code}' \
  --header "authorization: token ${TOKEN}" \
  --header "content-type: application/edn" \
  --header "accept: application/edn" \
  --data-binary "[{:name \"soak\" :store {:backend :file :id #uuid \"d4e7e5d8-8a43-4fc8-a5a0-60f7be8df66f\" :path \"${TMP_DIR}/database\"} :schema-flexibility :write :keep-history? true}]" \
  "http://127.0.0.1:${HTTP_PORT}/create-database")"
if [[ "${create_status}" != "200" ]]; then
  echo "create-database response (${create_status}):" >&2
  sed -n '1,80p' "${create_response}" >&2
  dump_log_and_fail "create-database failed"
fi
wait_for_pg

if PGPASSWORD=wrong PGCONNECT_TIMEOUT=3 PGSSLMODE=verify-full PGSSLROOTCERT="${CA_CERT}" \
  "${PSQL}" --no-psqlrc --host localhost --port "${PG_PORT}" \
  --username app --dbname soak --command "SELECT 1" >/dev/null 2>&1; then
  dump_log_and_fail "wrong PostgreSQL password was accepted"
fi
if PGPASSWORD="${PG_PASSWORD}" PGCONNECT_TIMEOUT=3 PGSSLMODE=disable \
  "${PSQL}" --no-psqlrc --host localhost --port "${PG_PORT}" \
  --username app --dbname soak --command "SELECT 1" >/dev/null 2>&1; then
  dump_log_and_fail "non-TLS PostgreSQL connection was accepted"
fi

psql_tls --set ON_ERROR_STOP=1 --command \
  "CREATE TABLE server_soak (id INTEGER PRIMARY KEY, note TEXT); INSERT INTO server_soak VALUES (1, 'before restart');" >/dev/null

# Repeatedly kill clients while a server-side statement is in flight. The
# listener must release each session and remain protocol-responsive.
for _ in $(seq 1 8); do
  psql_tls --command "SELECT pg_sleep(5)" >/dev/null 2>&1 &
  client_pid=$!
  sleep 0.15
  kill -KILL "${client_pid}" 2>/dev/null || true
  wait "${client_pid}" 2>/dev/null || true
done
[[ "$(psql_tls --tuples-only --no-align --command "SELECT count(*) FROM server_soak")" == "1" ]] \
  || dump_log_and_fail "listener did not recover after abrupt client drops"

stop_server
start_server
wait_for_pg

persisted="$(psql_tls --tuples-only --no-align --command \
  "SELECT id || ':' || note FROM server_soak ORDER BY id")"
[[ "${persisted}" == "1:before restart" ]] \
  || dump_log_and_fail "database row did not survive server restart"
psql_tls --set ON_ERROR_STOP=1 --command \
  "INSERT INTO server_soak VALUES (2, 'after restart');" >/dev/null
[[ "$(psql_tls --tuples-only --no-align --command "SELECT count(*) FROM server_soak")" == "2" ]] \
  || dump_log_and_fail "listener was not writable after restart"

stop_server

echo "PASS datahike-server-jar-soak"
echo "  datahike=${embedded_datahike_version} pg-datahike=${embedded_pg_version}"
echo "  sha256=$(sha256sum "${JAR}" | awk '{print $1}')"
echo "  verified=restart,persistence,tls,password-auth,abrupt-client-drop"
