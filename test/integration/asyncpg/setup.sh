#!/usr/bin/env bash
# Idempotent setup for the asyncpg wire-protocol regression suite.
#
# What it does:
#   1. Verifies git, python3 (>= 3.9), pip are available.
#   2. Clones asyncpg at a known-good tag into ./asyncpg/ (skip if present).
#   3. Creates a local virtualenv in ./.venv/.
#   4. Builds and installs asyncpg (incl. C extensions) from the clone,
#      plus the test-group dependencies (pytest, uvloop, etc.).
#
# This script does NOT start the pgwire server and does NOT run the tests.
# Use run.sh for that.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_URL="https://github.com/MagicStack/asyncpg.git"
REPO_TAG="v0.30.0"                       # latest stable at time of writing
CLONE_DIR="${HERE}/asyncpg"
VENV_DIR="${HERE}/.venv"

# ---- dependency check -------------------------------------------------------

have() { command -v "$1" >/dev/null 2>&1; }
require() {
  if ! have "$1"; then
    echo "ERROR: required tool '$1' not found on PATH." >&2
    exit 2
  fi
}

require git
require python3

PY_MAJMIN="$(python3 -c 'import sys; print("%d.%d" % sys.version_info[:2])')"
PY_OK="$(python3 -c 'import sys; print(1 if sys.version_info[:2] >= (3,9) else 0)')"
if [[ "${PY_OK}" != "1" ]]; then
  echo "ERROR: asyncpg requires Python >= 3.9 (found ${PY_MAJMIN})." >&2
  exit 2
fi

# A working C toolchain is needed because asyncpg has Cython extensions.
if ! have cc && ! have gcc; then
  echo "ERROR: no C compiler (cc/gcc) on PATH; asyncpg has C extensions." >&2
  exit 2
fi

# ---- clone ------------------------------------------------------------------

if [[ -d "${CLONE_DIR}/.git" ]]; then
  echo "[setup] asyncpg already cloned at ${CLONE_DIR} (tag: $(git -C "${CLONE_DIR}" describe --tags --always 2>/dev/null || echo '?'))"
else
  echo "[setup] cloning asyncpg ${REPO_TAG} into ${CLONE_DIR}"
  # Cython ext build needs the full submodule tree (libpq shipped headers etc.).
  git clone --depth=1 --branch="${REPO_TAG}" --recurse-submodules "${REPO_URL}" "${CLONE_DIR}"
fi

# ---- virtualenv -------------------------------------------------------------

if [[ ! -d "${VENV_DIR}" ]]; then
  echo "[setup] creating virtualenv at ${VENV_DIR}"
  python3 -m venv "${VENV_DIR}"
fi

# Invoke the environment's interpreter directly. Activation scripts embed the
# absolute path from venv creation time, so a renamed/moved checkout otherwise
# prepends a dead directory and silently falls back to a global `python`.
VENV_PYTHON="${VENV_DIR}/bin/python"
if [[ ! -x "${VENV_PYTHON}" ]]; then
  echo "ERROR: virtualenv has no executable Python: ${VENV_PYTHON}" >&2
  exit 2
fi

"${VENV_PYTHON}" -m pip install --upgrade --quiet pip wheel
# Pin setuptools < 81 — asyncpg v0.30.0's setup.py imports pkg_resources
# which setuptools 81 removed. Build isolation takes the latest by
# default, so we also pass --no-build-isolation below to force use of
# this pinned version during the C-ext compile.
"${VENV_PYTHON}" -m pip install --quiet 'setuptools<81' cython

# ---- install asyncpg + test deps --------------------------------------------
#
# We install from the clone in editable mode so that a git pull + rebuild is
# enough to pick up upstream changes. The `[test]` extra is defined in the
# upstream pyproject.toml.

if "${VENV_PYTHON}" -c 'import asyncpg' 2>/dev/null; then
  echo "[setup] asyncpg already installed in venv"
else
  echo "[setup] building and installing asyncpg from source (this compiles C ext)"
  (cd "${CLONE_DIR}" && "${VENV_PYTHON}" -m pip install --quiet --no-build-isolation -e .)
fi

# The test group is declared under [dependency-groups] in pyproject.toml,
# which `pip install -e .` does not pick up. Keep exact versions here so a
# fresh CI environment and a reused developer checkout collect and report the
# same verdict set.
"${VENV_PYTHON}" -m pip install --quiet \
  'pytest==9.1.1' \
  'pytest-timeout==2.4.0' \
  'uvloop==0.22.1' \
  'distro==1.9.0'

echo "[setup] done. Start the pgwire server, then run:  ./run.sh"
