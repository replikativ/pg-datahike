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
person_file="${postgres_source}/src/test/regress/data/person.data"
emp_file="${postgres_source}/src/test/regress/data/emp.data"
student_file="${postgres_source}/src/test/regress/data/student.data"
stud_emp_file="${postgres_source}/src/test/regress/data/stud_emp.data"

if [[ ! -x "${psql}" ]]; then
  echo "psql not executable under PG_REGRESS_BINDIR: ${pg_bindir}" >&2
  exit 2
fi

for required in "${onek_file}" "${tenk_file}" "${person_file}" \
  "${emp_file}" "${student_file}" "${stud_emp_file}"; do
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
    --command="\\copy ${table} FROM STDIN" < "${data_file}"
done

# test_setup's inheritance DDL is accepted only as a declared-column subset.
# Preserve the useful scalar fixtures without pretending inherited columns are
# implemented: load each child table's own columns from the corresponding
# upstream rows. road cannot be created until PostgreSQL path is supported.
"${psql}" -X -v ON_ERROR_STOP=1 \
  --host="${target_host}" \
  --port="${target_port}" \
  --username="${target_user}" \
  --dbname="${target_db}" \
  --command="\\copy person FROM STDIN" < "${person_file}"

awk -F '\t' 'BEGIN {OFS=FS} {print $4, $5}' "${emp_file}" \
  | "${psql}" -X -v ON_ERROR_STOP=1 \
    --host="${target_host}" --port="${target_port}" --username="${target_user}" \
    --dbname="${target_db}" --command="\\copy emp FROM STDIN"

awk -F '\t' '{print $4}' "${student_file}" \
  | "${psql}" -X -v ON_ERROR_STOP=1 \
    --host="${target_host}" --port="${target_port}" --username="${target_user}" \
    --dbname="${target_db}" --command="\\copy student FROM STDIN"

awk -F '\t' '{print $7}' "${stud_emp_file}" \
  | "${psql}" -X -v ON_ERROR_STOP=1 \
    --host="${target_host}" --port="${target_port}" --username="${target_user}" \
    --dbname="${target_db}" --command="\\copy stud_emp FROM STDIN"

expected_counts="1000|1000|10000|10000|50|3|2|3"
actual_counts="$("${psql}" -X -v ON_ERROR_STOP=1 --tuples-only --no-align \
  --field-separator='|' \
  --host="${target_host}" --port="${target_port}" --username="${target_user}" \
  --dbname="${target_db}" \
  --command="SELECT (SELECT count(*) FROM onek),
                    (SELECT count(*) FROM onek2),
                    (SELECT count(*) FROM tenk1),
                    (SELECT count(*) FROM tenk2),
                    (SELECT count(*) FROM person),
                    (SELECT count(*) FROM emp),
                    (SELECT count(*) FROM student),
                    (SELECT count(*) FROM stud_emp)")"

if [[ "${actual_counts}" != "${expected_counts}" ]]; then
  echo "fixture row-count verification failed" >&2
  echo "expected: ${expected_counts}" >&2
  echo "actual:   ${actual_counts}" >&2
  exit 1
fi
echo "Verified fixture rows: ${actual_counts}"
