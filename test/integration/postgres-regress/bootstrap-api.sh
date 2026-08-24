#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../../.." && pwd)"
postgres_source="${POSTGRES_SOURCE:-${repo_root}/../postgres}"
pg_major="${PG_REGRESS_MAJOR:-17}"
pg_bindir="${PG_REGRESS_BINDIR:-/usr/lib/postgresql/${pg_major}/bin}"
target_host="${PG_REGRESS_HOST:-127.0.0.1}"
target_port="${PG_REGRESS_PORT:-15432}"
target_user="${PG_REGRESS_USER:-datahike}"
target_db="${PG_REGRESS_DB:-datahike}"

psql="${pg_bindir}/psql"
onek_file="${postgres_source}/src/test/regress/data/onek.data"
tenk_file="${postgres_source}/src/test/regress/data/tenk.data"

for required in "${psql}" "${onek_file}" "${tenk_file}"; do
  if [[ ! -e "${required}" ]]; then
    echo "required PostgreSQL regression fixture not found: ${required}" >&2
    exit 2
  fi
done

echo "Bootstrapping API regression fixtures into ${target_host}:${target_port}/${target_db}"
"${psql}" -X -v ON_ERROR_STOP=1 \
  --host="${target_host}" \
  --port="${target_port}" \
  --username="${target_user}" \
  --dbname="${target_db}" \
  --file="${script_dir}/bootstrap-api.sql"

for table_and_file in \
  "onek|${onek_file}" \
  "onek2|${onek_file}" \
  "tenk1|${tenk_file}" \
  "tenk2|${tenk_file}"; do
  table="${table_and_file%%|*}"
  data_file="${table_and_file#*|}"
  "${psql}" -X -v ON_ERROR_STOP=1 \
    --host="${target_host}" \
    --port="${target_port}" \
    --username="${target_user}" \
    --dbname="${target_db}" \
    --command="\\copy ${table} FROM '${data_file}'"
done

"${psql}" -X -v ON_ERROR_STOP=1 \
  --host="${target_host}" \
  --port="${target_port}" \
  --username="${target_user}" \
  --dbname="${target_db}" \
  --command="SELECT (SELECT count(*) FROM onek) AS onek_rows,
                    (SELECT count(*) FROM onek2) AS onek2_rows,
                    (SELECT count(*) FROM tenk1) AS tenk1_rows,
                    (SELECT count(*) FROM tenk2) AS tenk2_rows"
