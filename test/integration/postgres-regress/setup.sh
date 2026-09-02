#!/usr/bin/env bash
# Materialize the exact PostgreSQL source tree used by campaign.edn without
# modifying a maintainer's existing ../postgres checkout.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "${HERE}/../../.." && pwd)"
REF="$(sed -n 's/.*:postgres-ref "\([^"]*\)".*/\1/p' "${HERE}/campaign.edn" | head -1)"
TARGET="${1:-${POSTGRES_SOURCE:-${ROOT}/.internal/postgres-${REF}}}"

if [[ -z "${REF}" ]]; then
  echo "ERROR: could not read :postgres-ref from ${HERE}/campaign.edn" >&2
  exit 2
fi

if [[ -d "${TARGET}/.git" ]]; then
  ACTUAL="$(git -C "${TARGET}" describe --tags --exact-match HEAD 2>/dev/null || true)"
  if [[ "${ACTUAL}" != "${REF}" ]]; then
    echo "ERROR: ${TARGET} is at ${ACTUAL:-an untagged commit}, expected ${REF}" >&2
    echo "Choose a new empty target; this script will not rewrite an existing checkout." >&2
    exit 2
  fi
  if ! git -C "${TARGET}" diff --quiet HEAD -- src/test/regress/parallel_schedule src/test/regress/sql; then
    echo "ERROR: ${TARGET} has modified PostgreSQL regression sources" >&2
    exit 2
  fi
  echo "PostgreSQL campaign source ready: ${TARGET} (${REF})"
  exit 0
fi

if [[ -e "${TARGET}" ]]; then
  echo "ERROR: target exists but is not a git checkout: ${TARGET}" >&2
  exit 2
fi

mkdir -p "$(dirname "${TARGET}")"
git clone --depth 1 --branch "${REF}" \
  https://github.com/postgres/postgres.git "${TARGET}"
echo "PostgreSQL campaign source ready: ${TARGET} (${REF})"
