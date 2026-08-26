#!/usr/bin/env bash
set -euo pipefail

# Run relation-dependent PostgreSQL regression tests in one disposable
# database. PostgreSQL's test_setup uses server-side COPY, so run its DDL,
# replace the file-loading portion with client-side \copy, and only then run
# the requested tests.

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../../.." && pwd)"
pg_major="${PG_REGRESS_MAJOR:-17}"
pg_bindir="${PG_REGRESS_BINDIR:-/usr/lib/postgresql/${pg_major}/bin}"
target_host="${PG_REGRESS_HOST:-127.0.0.1}"
target_port="${PG_REGRESS_PORT:-15432}"
target_user="${PG_REGRESS_USER:-datahike}"
admin_db="${PG_REGRESS_DB:-datahike}"
isolated_db="pgdh_regress_fixture_$(date -u +%Y%m%d%H%M%S)_$$_${RANDOM}"
database_created=0
psql="${pg_bindir}/psql"

if [[ $# -eq 0 ]]; then
  echo "usage: run-with-api-fixtures.sh TEST [TEST ...]" >&2
  exit 2
fi

for test_name in "$@"; do
  if [[ "${test_name}" == "test_setup" ]]; then
    echo "test_setup is implicit in the fixture runner; do not pass it as a target" >&2
    exit 2
  fi
done

if [[ ! -x "${psql}" ]]; then
  echo "psql not executable under PG_REGRESS_BINDIR: ${pg_bindir}" >&2
  exit 2
fi

if [[ -n "${PG_REGRESS_OUTPUT:-}" ]]; then
  output_dir="${PG_REGRESS_OUTPUT}"
else
  run_stamp="$(date -u +%Y%m%dT%H%M%SZ)-$$"
  output_dir="${repo_root}/.internal/pg-regress/${run_stamp}"
fi
mkdir -p "${output_dir}/setup" "${output_dir}/tests"

cleanup_isolated_db() {
  if [[ "${database_created}" == "1" ]]; then
    "${psql}" -X \
      --host="${target_host}" --port="${target_port}" --username="${target_user}" \
      --dbname="${admin_db}" --set=ON_ERROR_STOP=1 \
      --command="DROP DATABASE IF EXISTS \"${isolated_db}\"" >/dev/null || true
  fi
}
trap cleanup_isolated_db EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

echo "Creating disposable fixture database ${isolated_db} (admin: ${admin_db})"
"${psql}" -X \
  --host="${target_host}" --port="${target_port}" --username="${target_user}" \
  --dbname="${admin_db}" --set=ON_ERROR_STOP=1 \
  --command="CREATE DATABASE \"${isolated_db}\"" >/dev/null
database_created=1

# test_setup is expected to differ because pg-datahike deliberately rejects
# its server-side COPY FROM paths. Never let a caller's strict flags turn that
# classified setup difference into a failed target run.
env -u PG_REGRESS_STRICT -u PG_REGRESS_API_STRICT \
  PG_REGRESS_DB="${isolated_db}" \
  PG_REGRESS_ISOLATE=0 \
  PG_REGRESS_OUTPUT="${output_dir}/setup" \
  bash "${script_dir}/run.sh" test_setup

PG_REGRESS_DB="${isolated_db}" \
  bash "${script_dir}/bootstrap-api.sh" 2>&1 | tee "${output_dir}/bootstrap.log"

echo "Running fixture-dependent tests: $*"
PG_REGRESS_DB="${isolated_db}" \
PG_REGRESS_ISOLATE=0 \
PG_REGRESS_OUTPUT="${output_dir}/tests" \
  bash "${script_dir}/run.sh" "$@"
