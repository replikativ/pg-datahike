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

# The inheritance fixtures, loaded the way test_setup loads them: whole
# rows, inherited columns included. These used to be awk-ed down to each
# child's OWN columns, because a child's inherited columns were invisible
# and a full row would have been rejected -- so the fixture agreed with
# the bug rather than with PostgreSQL, and `person` never saw the rows
# its descendants held. road still cannot be created (PostgreSQL `path`).
for table_and_file in \
  "person|${person_file}" \
  "emp|${emp_file}" \
  "student|${student_file}" \
  "stud_emp|${stud_emp_file}"; do
  table="${table_and_file%%|*}"
  data_file="${table_and_file#*|}"
  "${psql}" -X -v ON_ERROR_STOP=1 \
    --host="${target_host}" \
    --port="${target_port}" \
    --username="${target_user}" \
    --dbname="${target_db}" \
    --command="\\copy ${table} FROM STDIN" < "${data_file}"
done

# person/emp/student/stud_emp count 58|6|5|3, not 50|3|2|3: a query on a
# table includes its descendants' rows, so person holds its own 50 plus
# the 6 its children loaded, and emp and student each hold stud_emp's 3.
# These are real PostgreSQL 17's counts for the same four files.
expected_counts="1000|1000|10000|10000|58|6|5|3"
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
