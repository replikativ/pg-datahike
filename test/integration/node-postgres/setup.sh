#!/usr/bin/env bash
# Idempotent setup for the node-postgres (pg) integration test suite.
#
# What it does:
#   1. Verifies git, node (>= 18), and either yarn or npm are available.
#   2. Clones node-postgres at a known-good tag into ./node-postgres/
#      (skip if already there).
#   3. Installs workspace deps and builds the TypeScript sources.
#
# This script does NOT start the pgwire server and does NOT run the tests.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_URL="https://github.com/brianc/node-postgres.git"
# node-postgres uses the `pg@8.y.z` tags for releases. We pin to a recent tag
# that works with node >= 18. Update cautiously - the test harness format has
# occasionally shifted.
REPO_REF="pg@8.14.1"
CLONE_DIR="${HERE}/node-postgres"

# ---- dependency check -------------------------------------------------------

have() { command -v "$1" >/dev/null 2>&1; }
require() {
  if ! have "$1"; then
    echo "ERROR: required tool '$1' not found on PATH." >&2
    exit 2
  fi
}

require git
require node
require npm

NODE_MAJOR="$(node --version | sed 's/^v//' | cut -d. -f1)"
if [[ "${NODE_MAJOR}" -lt 18 ]]; then
  echo "ERROR: node-postgres tests require Node >= 18 (found v${NODE_MAJOR})." >&2
  exit 2
fi

# yarn is preferred because the monorepo uses yarn workspaces + lerna, but we
# fall back to corepack-provided yarn or npm install if necessary.
if have yarn; then
  PKG=yarn
elif have corepack; then
  # corepack ships with node >= 16.10 and provides yarn on demand.
  corepack enable >/dev/null 2>&1 || true
  if have yarn; then
    PKG=yarn
  else
    PKG=npm
  fi
else
  PKG=npm
fi

# ---- clone ------------------------------------------------------------------

if [[ -d "${CLONE_DIR}/.git" ]]; then
  echo "[setup] node-postgres already cloned at ${CLONE_DIR} (ref: $(git -C "${CLONE_DIR}" describe --tags --always 2>/dev/null || echo '?'))"
else
  echo "[setup] cloning node-postgres ${REPO_REF} into ${CLONE_DIR}"
  # The repo does not have a lightweight REL branch; we clone shallow with the
  # tag ref directly.
  git clone --depth=1 --branch="${REPO_REF}" "${REPO_URL}" "${CLONE_DIR}" 2>/dev/null \
    || {
      echo "[setup] tag ${REPO_REF} not fetchable as shallow ref; falling back to full clone"
      git clone "${REPO_URL}" "${CLONE_DIR}"
      git -C "${CLONE_DIR}" checkout "${REPO_REF}"
    }
fi

# ---- install & build --------------------------------------------------------

cd "${CLONE_DIR}"

# Sentinel: only re-install if node_modules is missing or the lockfile changed.
NEED_INSTALL=1
if [[ -d "node_modules" ]] && [[ -d "packages/pg/lib" ]]; then
  NEED_INSTALL=0
fi

if [[ "${NEED_INSTALL}" -eq 1 ]]; then
  echo "[setup] installing workspace deps (this takes 1-2 minutes)"
  if [[ "${PKG}" == "yarn" ]]; then
    yarn install --frozen-lockfile
  else
    # `npm install` handles yarn workspaces well enough for this repo.
    npm install --no-audit --no-fund --loglevel=error
  fi

  echo "[setup] building TypeScript sources"
  # `yarn build` / `npm run build` runs `tsc --build` at the workspace root.
  if [[ "${PKG}" == "yarn" ]]; then
    yarn build
  else
    npm run build
  fi
else
  echo "[setup] workspace deps already installed; skipping install + build"
fi

echo "[setup] done. Start the pgwire server, then run:  ./run.sh"
