#!/usr/bin/env bash
set -euo pipefail

# Run PostgreSQL's own pg_regress driver against an already-running
# pg-datahike endpoint. Differences are the expected starting state: exit 1
# from pg_regress means "the test ran and produced a diff", so it is
# non-fatal unless PG_REGRESS_STRICT=1. Exit 2 means the harness itself could
# not run and always remains fatal.

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../../.." && pwd)"
postgres_source="${POSTGRES_SOURCE:-${repo_root}/../postgres}"
pg_major="${PG_REGRESS_MAJOR:-17}"
pg_regress="${PG_REGRESS_BIN:-/usr/lib/postgresql/${pg_major}/lib/pgxs/src/test/regress/pg_regress}"
pg_bindir="${PG_REGRESS_BINDIR:-/usr/lib/postgresql/${pg_major}/bin}"
target_host="${PG_REGRESS_HOST:-127.0.0.1}"
target_port="${PG_REGRESS_PORT:-15432}"
target_user="${PG_REGRESS_USER:-datahike}"
target_db="${PG_REGRESS_DB:-datahike}"

if [[ $# -eq 0 ]]; then
  echo "usage: bb pg-regress TEST [TEST ...]" >&2
  echo "example: PG_REGRESS_PORT=15436 bb pg-regress jsonb" >&2
  exit 2
fi

if [[ ! -x "${pg_regress}" ]]; then
  echo "pg_regress not executable: ${pg_regress}" >&2
  echo "set PG_REGRESS_BIN to PostgreSQL's pg_regress binary" >&2
  exit 2
fi

if [[ ! -x "${pg_bindir}/psql" ]]; then
  echo "psql not executable under PG_REGRESS_BINDIR: ${pg_bindir}" >&2
  exit 2
fi

input_dir="${postgres_source}/src/test/regress"
if [[ ! -d "${input_dir}/sql" || ! -d "${input_dir}/expected" ]]; then
  echo "PostgreSQL regression sources not found under: ${input_dir}" >&2
  echo "set POSTGRES_SOURCE to a PostgreSQL source checkout" >&2
  exit 2
fi

if [[ -n "${PG_REGRESS_OUTPUT:-}" ]]; then
  output_dir="${PG_REGRESS_OUTPUT}"
else
  run_stamp="$(date -u +%Y%m%dT%H%M%SZ)-$$"
  output_dir="${repo_root}/.internal/pg-regress/${run_stamp}"
fi
mkdir -p "${output_dir}"

echo "PostgreSQL source: ${postgres_source}"
echo "Target:            ${target_host}:${target_port}/${target_db} as ${target_user}"
echo "Tests:             $*"
echo "Artifacts:         ${output_dir}"

set +e
"${pg_regress}" \
  --use-existing \
  --host="${target_host}" \
  --port="${target_port}" \
  --user="${target_user}" \
  --dbname="${target_db}" \
  --bindir="${pg_bindir}" \
  --inputdir="${input_dir}" \
  --expecteddir="${input_dir}" \
  --outputdir="${output_dir}" \
  "$@"
regress_status=$?
set -e

diff_file="${output_dir}/regression.diffs"
if [[ -f "${diff_file}" ]]; then
  echo
  echo "Diff lines: $(wc -l < "${diff_file}")"
fi

shopt -s nullglob
result_files=("${output_dir}"/results/*.out)
if (( ${#result_files[@]} > 0 )); then
  echo
  echo "Most frequent target errors:"
  rg --no-filename '^ERROR:  .+' "${result_files[@]}" \
    | sort | uniq -c | sort -nr | head -20 || true

  echo
  echo "Internal-failure signatures (first 30):"
  rg -n --no-heading \
    'class .* cannot be cast|ClassCastException|NullPointerException|Query for unknown vars|SQLSTATE XX000|server closed the connection' \
    "${result_files[@]}" | head -30 || true
fi

case "${regress_status}" in
  0)
    echo
    echo "PostgreSQL regression tests matched their expected output."
    ;;
  1)
    echo
    echo "Regression differences recorded (expected during compatibility work)."
    if [[ "${PG_REGRESS_STRICT:-0}" == "1" ]]; then
      exit 1
    fi
    ;;
  *)
    echo
    echo "pg_regress could not run successfully (status ${regress_status})." >&2
    exit "${regress_status}"
    ;;
esac
