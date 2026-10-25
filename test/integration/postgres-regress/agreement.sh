#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "${ROOT}"
# The endpoint is externally managed. This wrapper never starts, stops or
# replaces a server. Raw outputs and reference receipts stay in the run directory.
exec bb test/integration/postgres-regress/agreement-runner.clj "$@"
