#!/usr/bin/env bash
# Idempotent setup for the Metabase integration harness.
#
# Downloads the Metabase JAR (~600 MB) into ./metabase.jar if not
# already present. Subsequent runs are a no-op.
#
# Does NOT start anything. Use run.sh after this completes.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JAR="${HERE}/metabase.jar"
URL="${METABASE_JAR_URL:-https://downloads.metabase.com/latest/metabase.jar}"

have() { command -v "$1" >/dev/null 2>&1; }
require() {
  if ! have "$1"; then
    echo "ERROR: required tool '$1' not found on PATH." >&2
    exit 2
  fi
}

require curl
require jq
require java

# Metabase 0.50+ wants Java 21. Auto-detect a JDK 21 if the active java
# is newer/older; fall back to PATH-java if no JDK 21 is installed.
JAVA_HOME_OVERRIDE="${METABASE_JDK_HOME:-}"
if [[ -z "${JAVA_HOME_OVERRIDE}" ]]; then
  for candidate in \
    "/usr/lib/jvm/java-21-openjdk-amd64" \
    "/usr/lib/jvm/temurin-21-jdk-amd64" \
    "/Library/Java/JavaVirtualMachines/temurin-21.jdk/Contents/Home"; do
    if [[ -x "${candidate}/bin/java" ]]; then
      JAVA_HOME_OVERRIDE="${candidate}"
      break
    fi
  done
fi
if [[ -n "${JAVA_HOME_OVERRIDE}" ]]; then
  echo "[setup] JDK 21 detected: ${JAVA_HOME_OVERRIDE}"
fi

if [[ -f "${JAR}" ]]; then
  echo "[setup] metabase.jar already present ($(du -h "${JAR}" | cut -f1))"
else
  echo "[setup] downloading ${URL}"
  curl -fL --progress-bar -o "${JAR}.partial" "${URL}"
  mv "${JAR}.partial" "${JAR}"
  echo "[setup] saved $(du -h "${JAR}" | cut -f1) to ${JAR}"
fi

echo "[setup] done. Start the seeded pgwire server + Metabase via:"
echo "          ./run.sh"
