#!/usr/bin/env bash
# Round-trip a NON-TRIVIAL database through PostgreSQL's own pg_dump and
# restore the result into pg-datahike.
#
# Why this exists, and why it uses the real binaries:
#
#   The repo already vendored a real pg_dump of pagila — but taken with
#   `--inserts`. The DEFAULT pg_dump format is COPY, and nothing
#   exercised it, so two decoding bugs survived: `timestamptz` in PG's
#   output form (`2022-01-28 17:58:52.222594-08` — space separator,
#   hour-only offset) was rejected outright, 380 times, and `bytea` hex
#   reached the transactor as a string. A default-format restore loaded
#   ZERO rows.
#
#   Generating the dump here rather than vendoring one means this tracks
#   whatever pg_dump the CI image ships, so the next format drift shows
#   up as a failure rather than as a fixture nobody regenerates.
#
# What it asserts: per-table row counts must MATCH the source PostgreSQL.
# It deliberately does NOT assert "zero errors" — pagila carries
# triggers, functions, partitions, a matview and an aggregate, all of
# which we reject on purpose. Asserting zero would mean either faking
# them or disabling the test.
#
# Requirements: a source PostgreSQL (PGDUMP_SRC_*), psql + pg_dump on
# PATH, and pg-datahike listening on localhost:15432.
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "${HERE}/../../.." && pwd)"
FIXTURES="${ROOT}/test/fixtures"
LOG="${HERE}/last-run.log"
RESTORE_LOG="${HERE}/last-restore.log"
DUMP="${HERE}/pagila-default-format.sql"

SRC_HOST="${PGDUMP_SRC_HOST:-127.0.0.1}"
SRC_PORT="${PGDUMP_SRC_PORT:-5432}"
SRC_USER="${PGDUMP_SRC_USER:-postgres}"
SRC_DB="${PGDUMP_SRC_DB:-postgres}"
export PGPASSWORD="${PGDUMP_SRC_PASSWORD:-postgres}"

DH_HOST="${PGWIRE_HOST:-127.0.0.1}"
DH_PORT="${PGWIRE_PORT:-15432}"
DH_USER="${PGWIRE_USER:-datahike}"
DH_DB="${PGWIRE_DB:-datahike}"

for bin in psql pg_dump; do
  command -v "$bin" >/dev/null 2>&1 || { echo "ERROR: $bin not on PATH" >&2; exit 2; }
done
if ! (exec 3<>/dev/tcp/"${DH_HOST}"/"${DH_PORT}") 2>/dev/null; then
  echo "ERROR: cannot connect to pg-datahike at ${DH_HOST}:${DH_PORT}" >&2; exit 2
fi
exec 3<&-; exec 3>&-

src()  { psql -h "${SRC_HOST}" -p "${SRC_PORT}" -U "${SRC_USER}" -d "${SRC_DB}" "$@"; }
dh()   { PGPASSWORD="${PGWIRE_PASSWORD:-datahike}" psql -h "${DH_HOST}" -p "${DH_PORT}" \
             -U "${DH_USER}" -d "${DH_DB}" "$@"; }

: > "${LOG}"

echo "[1/4] seeding source PostgreSQL with pagila"
src -q -c "DROP SCHEMA IF EXISTS public CASCADE; CREATE SCHEMA public;" >>"${LOG}" 2>&1
src -q -f "${FIXTURES}/pagila-schema.sql" >>"${LOG}" 2>&1
src -q -f "${FIXTURES}/pagila-data.sql"   >>"${LOG}" 2>&1

echo "[2/4] pg_dump (DEFAULT format — COPY, not --inserts)"
pg_dump -h "${SRC_HOST}" -p "${SRC_PORT}" -U "${SRC_USER}" -d "${SRC_DB}" \
        --no-owner --no-privileges > "${DUMP}" 2>>"${LOG}"
copy_blocks=$(grep -c '^COPY ' "${DUMP}")
echo "      $(wc -l < "${DUMP}") lines, ${copy_blocks} COPY blocks"
if [[ "${copy_blocks}" -lt 10 ]]; then
  echo "ERROR: dump has ${copy_blocks} COPY blocks — expected the default format" >&2
  exit 1
fi

echo "[3/4] restoring into pg-datahike"
# Its OWN log: the seeding steps above also write errors, and counting
# them together made the restore look far worse than it is.
dh -q -f "${DUMP}" > "${RESTORE_LOG}" 2>&1
cat "${RESTORE_LOG}" >> "${LOG}"
ddl_errors=$(grep -c 'ERROR' "${RESTORE_LOG}")
echo "      ${ddl_errors} statement errors (unsupported DDL is expected)"
# A DATA error is never expected — unsupported DDL is, but a COPY that
# fails mid-stream means a decoding bug, which is exactly what this
# suite exists to catch. Surface those separately.
data_errors=$(grep -c 'COPY failed' "${RESTORE_LOG}")
if [[ "${data_errors}" -ne 0 ]]; then
  echo "  ${data_errors} COPY block(s) FAILED — a data-decoding regression:"
  grep 'COPY failed' "${RESTORE_LOG}" | head -5 | sed 's/^/    /'
fi

echo "[4/4] comparing per-table row counts"
# Partitioned parents are excluded: ATTACH PARTITION is unsupported, so
# our rows live in the partition tables and the parent is legitimately
# empty. Compare the partitions themselves instead.
tables=$(src -At -c "
  SELECT c.relname FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
   WHERE n.nspname = 'public' AND c.relkind = 'r' AND NOT c.relispartition
     AND c.oid NOT IN (SELECT inhparent FROM pg_inherits)
   ORDER BY 1")

fail="${data_errors:-0}"; checked=0
for t in ${tables}; do
  s=$(src -At -c "SELECT count(*) FROM \"${t}\"" 2>/dev/null | head -1)
  d=$(dh  -At -c "SELECT count(*) FROM \"${t}\"" 2>/dev/null | head -1)
  checked=$((checked + 1))
  if [[ "${s}" != "${d}" ]]; then
    echo "  MISMATCH ${t}: source=${s} pg-datahike=${d}"
    fail=$((fail + 1))
  fi
done

echo
echo "SUMMARY: ${checked} tables compared, ${fail} mismatched"
if [[ "${fail}" -ne 0 ]]; then
  echo "Full output in ${LOG}"
  exit 1
fi
echo "pg_dump round-trip OK"
