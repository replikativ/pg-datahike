#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
pg_major="${PG_REGRESS_MAJOR:-17}"
pg_bindir="${PG_REGRESS_BINDIR:-/usr/lib/postgresql/${pg_major}/bin}"
target_host="${PG_REGRESS_HOST:-127.0.0.1}"
target_port="${PG_REGRESS_PORT:-15432}"
target_user="${PG_REGRESS_USER:-datahike}"
target_db="${PG_REGRESS_DB:-datahike}"

psql="${pg_bindir}/psql"
if [[ ! -x "${psql}" ]]; then
  echo "psql not executable under PG_REGRESS_BINDIR: ${pg_bindir}" >&2
  exit 2
fi

# Nothing is loaded here any more. `test_setup` loads all eight fixtures
# itself, from its own server-side `COPY ... FROM 'file'`, and builds
# onek2/tenk2 with CTAS -- so this script's whole job is now to say so,
# and to fail loudly if that stops being true.
#
# It did load them, and when server-side COPY started working the rows
# went in TWICE: 2000 where PostgreSQL has 1000. The count check below
# is what caught it, which is the argument for keeping it.
echo "Verifying test_setup fixtures in ${target_host}:${target_port}/${target_db}"

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
