#!/usr/bin/env bash
set -euo pipefail
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [[ "${PG_REGRESS_STRICT:-0}" == 1 || "${PG_REGRESS_API_STRICT:-0}" == 1 ]]; then
  echo "PostgreSQL 18 fixtures are raw discovery; use named run.sh for strict assertions" >&2
  exit 2
fi
source "${script_dir}/reference.sh"
output_dir="$(bb "${script_dir}/reference.clj" output)"
phase_file="$(mktemp)"
if ! bb "${script_dir}/fixtures.clj" plan "${output_dir}" "$@" > "${phase_file}"; then
  rm -f "${phase_file}"
  exit 2
fi
isolated_db=regression
database_created=0
database_attempted=0
bootstrap_status=incomplete
finish_fixture() {
  run_status=$?
  trap - EXIT
  cleanup_status=not-created
  if [[ "${database_attempted}" == 1 ]]; then cleanup_status=unproven; fi
  if [[ "${database_created}" == 1 ]]; then
    bb "${script_dir}/fixtures.clj" cleanup-start "${output_dir}"
    if bb "${script_dir}/fixtures.clj" cleanup-database "${output_dir}" > "${output_dir}/cleanup.log" 2>&1; then
      cleanup_status=complete
    else
      cleanup_status=failed
    fi
  fi
  bb "${script_dir}/fixtures.clj" cleanup-global "${output_dir}" "${cleanup_status}"
  rm -f "${phase_file}"
  if ! bb "${script_dir}/fixtures.clj" finish "${output_dir}" "${run_status}" \
    "${bootstrap_status}" "${cleanup_status}" "${isolated_db}"; then
    if [[ "${run_status}" == 0 ]]; then run_status=2; fi
  fi
  exit "${run_status}"
}
trap finish_fixture EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
bb "${script_dir}/fixtures.clj" pending "${output_dir}" "${isolated_db}"
bb "${script_dir}/fixtures.clj" preflight "${output_dir}"
database_attempted=1
reference_psql -X --host="${PG_REGRESS_HOST:-127.0.0.1}" --port="${PG_REGRESS_PORT:-15432}" \
  --username="${PG_REGRESS_USER:-datahike}" --dbname="${PG_REGRESS_DB:-datahike}" \
  --set=ON_ERROR_STOP=1 --command="CREATE DATABASE \"${isolated_db}\"" > "${output_dir}/create.log" 2>&1
database_created=1
bb "${script_dir}/fixtures.clj" database-created "${output_dir}"
export _PG_REGRESS_DLPATH="$(bb "${script_dir}/fixtures.clj" dlpath "${output_dir}")"
while IFS= read -r -d '' phase_name && IFS= read -r -d '' phase_directory && IFS= read -r -d '' phase_kind && IFS= read -r -d '' phase_timeout && IFS= read -r -d '' phase_process_timeout; do
  env -u PG_REGRESS_STRICT -u PG_REGRESS_API_STRICT \
    PG_REGRESS_DB="${isolated_db}" PG_REGRESS_ISOLATE=0 \
    PG_REGRESS_TIMEOUT="${phase_process_timeout}" PG_REGRESS_STATEMENT_TIMEOUT="${phase_timeout}" PG_REGRESS_OUTPUT="${output_dir}/${phase_directory}" \
    bash "${script_dir}/run.sh" "${phase_name}" 2>&1 | tee "${output_dir}/${phase_name}.log"
  if [[ "${phase_kind}" == setup ]]; then
    PG_REGRESS_DB="${isolated_db}" bash "${script_dir}/bootstrap-api.sh" > "${output_dir}/bootstrap.log" 2>&1
    bootstrap_status=complete
  fi
done < "${phase_file}"
