#!/usr/bin/env bash
# Release soak for the non-root Datahike Server Docker/Podman image.
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 IMAGE" >&2
  exit 2
fi

IMAGE="$1"
EXPECTED_PG_VERSION="${EXPECTED_PG_DATAHIKE_VERSION:-0.1.189}"
EXPECTED_DATAHIKE_VERSION="${EXPECTED_DATAHIKE_VERSION:-}"
PSQL="${PSQL:-psql}"
ENGINE="${DATAHIKE_CONTAINER_ENGINE:-}"
if [[ -z "${ENGINE}" ]]; then
  if command -v docker >/dev/null; then ENGINE=docker
  elif command -v podman >/dev/null; then ENGINE=podman
  else echo "ERROR: Docker or Podman is required" >&2; exit 2
  fi
fi
for command in curl keytool sha256sum unzip "${PSQL}" "${ENGINE}"; do
  command -v "${command}" >/dev/null || {
    echo "ERROR: required command not found: ${command}" >&2
    exit 2
  }
done

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

run_id="$(tr -d '-' </proc/sys/kernel/random/uuid)"
name="pg-datahike-soak-${run_id}"
volume="pg-datahike-soak-${run_id}"
HTTP_PORT="${DATAHIKE_SOAK_HTTP_PORT:-$(free_port 18544 18643)}"
PG_PORT="${DATAHIKE_SOAK_PG_PORT:-$(free_port 15539 15638)}"
TMP_DIR="$(mktemp -d /tmp/pg-datahike-container-soak.XXXXXX)"
CONFIG="${TMP_DIR}/server.edn"
KEYSTORE="${TMP_DIR}/server.p12"
CA_CERT="${TMP_DIR}/server.crt"
PACKAGED_JAR="${TMP_DIR}/datahike-http-server.jar"
TOKEN="datahike-container-soak-http-token"
PG_PASSWORD="datahike-container-soak-pg-password"

cleanup() {
  "${ENGINE}" rm --force --volumes "${name}" >/dev/null 2>&1 || true
  "${ENGINE}" volume rm --force "${volume}" >/dev/null 2>&1 || true
  rm -rf "${TMP_DIR}"
}
trap cleanup EXIT

image_user="$("${ENGINE}" image inspect --format '{{.Config.User}}' "${IMAGE}")"
[[ "${image_user}" == "10001:10001" ]] || {
  echo "ERROR: image user is ${image_user}, expected 10001:10001" >&2
  exit 1
}

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
{:host "0.0.0.0"
 :port 4444
 :token "${TOKEN}"
 :shutdown-timeout-ms 5000
 :system-db {:store {:backend :file :path "/var/lib/datahike/system"}}
 :pg-listener
 {:host "0.0.0.0"
  :port 5432
  :users {"app" "${PG_PASSWORD}"}
  :tls {:keystore "/run/secrets/server.p12"
        :keystore-password "changeit"}}}
EOF
chmod 755 "${TMP_DIR}"
chmod 644 "${CONFIG}" "${KEYSTORE}" "${CA_CERT}"

"${ENGINE}" volume create "${volume}" >/dev/null
"${ENGINE}" run --detach --name "${name}" \
  --publish "127.0.0.1:${HTTP_PORT}:4444" \
  --publish "127.0.0.1:${PG_PORT}:5432" \
  --mount "type=volume,source=${volume},target=/var/lib/datahike" \
  --mount "type=bind,source=${CONFIG},target=/run/secrets/server.edn,readonly" \
  --mount "type=bind,source=${KEYSTORE},target=/run/secrets/server.p12,readonly" \
  "${IMAGE}" --config /run/secrets/server.edn >/dev/null

# Inspect the exact JAR shipped inside the image, not the host-side build input.
"${ENGINE}" cp "${name}:/opt/datahike/datahike-http-server.jar" "${PACKAGED_JAR}"
embedded_pg_version="$(unzip -p "${PACKAGED_JAR}" pg-datahike.version)"
embedded_datahike_version="$(unzip -p "${PACKAGED_JAR}" DATAHIKE_VERSION)"
if [[ "${embedded_pg_version}" != "${EXPECTED_PG_VERSION}" ]]; then
  echo "ERROR: expected pg-datahike ${EXPECTED_PG_VERSION}, image contains ${embedded_pg_version}" >&2
  exit 1
fi
if [[ -n "${EXPECTED_DATAHIKE_VERSION}" &&
      "${embedded_datahike_version}" != "${EXPECTED_DATAHIKE_VERSION}" ]]; then
  echo "ERROR: expected Datahike ${EXPECTED_DATAHIKE_VERSION}, image contains ${embedded_datahike_version}" >&2
  exit 1
fi

dump_log_and_fail() {
  echo "ERROR: $1" >&2
  "${ENGINE}" logs --tail 120 "${name}" >&2 || true
  exit 1
}

wait_until_live() {
  local attempt
  for attempt in $(seq 1 480); do
    if curl --silent --fail "http://127.0.0.1:${HTTP_PORT}/health/live" >/dev/null; then
      return
    fi
    sleep 0.25
  done
  dump_log_and_fail "container did not become live within 120 seconds"
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
  dump_log_and_fail "container PostgreSQL listener did not accept TLS connections"
}

wait_until_live
create_response="${TMP_DIR}/create-response.txt"
create_status="$(curl --silent --show-error \
  --output "${create_response}" --write-out '%{http_code}' \
  --header "authorization: token ${TOKEN}" \
  --header "content-type: application/edn" \
  --header "accept: application/edn" \
  --data-binary '[{:name "soak" :store {:backend :file :id #uuid "969fc2d4-5e69-49a2-8cd6-183947142b63" :path "/var/lib/datahike/database"} :schema-flexibility :write :keep-history? true}]' \
  "http://127.0.0.1:${HTTP_PORT}/create-database")"
if [[ "${create_status}" != "200" ]]; then
  sed -n '1,80p' "${create_response}" >&2
  dump_log_and_fail "container create-database failed with HTTP ${create_status}"
fi
wait_for_pg

if PGPASSWORD=wrong PGCONNECT_TIMEOUT=3 PGSSLMODE=verify-full PGSSLROOTCERT="${CA_CERT}" \
  "${PSQL}" --no-psqlrc --host localhost --port "${PG_PORT}" \
  --username app --dbname soak --command "SELECT 1" >/dev/null 2>&1; then
  dump_log_and_fail "container accepted a wrong PostgreSQL password"
fi
if PGPASSWORD="${PG_PASSWORD}" PGCONNECT_TIMEOUT=3 PGSSLMODE=disable \
  "${PSQL}" --no-psqlrc --host localhost --port "${PG_PORT}" \
  --username app --dbname soak --command "SELECT 1" >/dev/null 2>&1; then
  dump_log_and_fail "container accepted a non-TLS PostgreSQL connection"
fi

psql_tls --set ON_ERROR_STOP=1 --command \
  "CREATE TABLE server_soak (id INTEGER PRIMARY KEY, note TEXT); INSERT INTO server_soak VALUES (1, 'before restart');" >/dev/null
for _ in $(seq 1 4); do
  psql_tls --command "SELECT pg_sleep(5)" >/dev/null 2>&1 &
  client_pid=$!
  sleep 0.15
  kill -KILL "${client_pid}" 2>/dev/null || true
  wait "${client_pid}" 2>/dev/null || true
done
[[ "$(psql_tls --tuples-only --no-align --command "SELECT count(*) FROM server_soak")" == "1" ]] \
  || dump_log_and_fail "container listener did not recover after abrupt client drops"

"${ENGINE}" stop --time 20 "${name}" >/dev/null
"${ENGINE}" start "${name}" >/dev/null
wait_until_live
wait_for_pg

persisted="$(psql_tls --tuples-only --no-align --command \
  "SELECT id || ':' || note FROM server_soak ORDER BY id")"
[[ "${persisted}" == "1:before restart" ]] \
  || dump_log_and_fail "container database row did not survive restart"
psql_tls --set ON_ERROR_STOP=1 --command \
  "INSERT INTO server_soak VALUES (2, 'after restart');" >/dev/null
[[ "$(psql_tls --tuples-only --no-align --command "SELECT count(*) FROM server_soak")" == "2" ]] \
  || dump_log_and_fail "container listener was not writable after restart"

"${ENGINE}" stop --time 20 "${name}" >/dev/null

echo "PASS datahike-server-container-soak"
echo "  engine=${ENGINE} image=${IMAGE} user=${image_user}"
echo "  datahike=${embedded_datahike_version} pg-datahike=${embedded_pg_version}"
echo "  jar-sha256=$(sha256sum "${PACKAGED_JAR}" | awk '{print $1}')"
echo "  verified=non-root,restart,persistence,tls,password-auth,abrupt-client-drop"
