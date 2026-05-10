#!/usr/bin/env bash
# One-time setup: install Odoo's pip requirements into the venv we'll
# point `run.sh` at. Idempotent — re-run is cheap once the venv has
# everything.
set -euo pipefail

ODOO_ROOT="${ODOO_ROOT:-/home/christian-weilbach/Development/odoo}"
ODOO_VENV="${ODOO_VENV:-${ODOO_ROOT}/.venv}"

if [[ ! -d "${ODOO_ROOT}" ]]; then
  echo "ERROR: ODOO_ROOT=${ODOO_ROOT} does not exist." >&2
  echo "Set ODOO_ROOT to your Odoo 19 checkout, or clone:" >&2
  echo "  git clone --depth 1 -b 19.0 https://github.com/odoo/odoo \"${ODOO_ROOT}\"" >&2
  exit 2
fi

if [[ ! -d "${ODOO_VENV}" ]]; then
  echo "[setup] creating venv at ${ODOO_VENV}"
  python3 -m venv "${ODOO_VENV}"
fi

# shellcheck disable=SC1091
source "${ODOO_VENV}/bin/activate"

# If odoo-bin already runs from the venv, treat it as already-set-up
# (pip install -r requirements.txt is heavy and pulls in extras like
# python-ldap that need system build deps the user may not have).
if PYTHONPATH="${ODOO_ROOT}" python "${ODOO_ROOT}/odoo-bin" --help \
     >/dev/null 2>&1; then
  echo "[setup] odoo-bin runs — venv looks complete."
else
  echo "[setup] odoo-bin not runnable — installing requirements.txt"
  python -m pip install --upgrade pip wheel --quiet
  python -m pip install -r "${ODOO_ROOT}/requirements.txt" --quiet
fi

# Verify the minimum surface run.sh needs.
# - psycopg2/lxml/babel/dateutil/werkzeug: required to boot Odoo at all.
# - stdnum: imported by addons/account/tools/structured_reference.py at
#   module load — `--init=account` (and any module depending on it)
#   breaks with `ImportError: Could not load the module 'stdnum' to
#   patch` if missing. Cheaper to fail here than ~5 min into module
#   loading. The optimistic "odoo-bin --help works" branch above skips
#   the full requirements install when only base was previously used.
python -c "import psycopg2, lxml, babel, dateutil, werkzeug, stdnum; print('[setup] minimum py deps OK')" \
  || { echo "ERROR: install psycopg2 lxml babel dateutil werkzeug python-stdnum" >&2; exit 1; }

echo "[setup] done. Run ./run.sh to execute the TestORM suite."
