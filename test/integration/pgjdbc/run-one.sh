#!/usr/bin/env bash
# Run a single pgjdbc test class for fast iteration.
#
# Usage:   ./run-one.sh <ClassName>   (e.g. ResultSetTest, BatchExecuteTest)
# Or:      ./run-one.sh <fqcn>        (org.postgresql.test.jdbc2.ResultSetTest)
#
# Requires: pgwire server on localhost:15432.
# Output:   last-run-one.log in this directory; summary line to stdout.
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLONE_DIR="${HERE}/pgjdbc"
LOG="${HERE}/last-run-one.log"

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <class>[.method] [...more filters]" >&2
  exit 2
fi

CLASS="$1"; shift
# Accept bare class names like "ResultSetTest" — map to jdbc2 by default.
# Leading "org." means "user supplied FQCN"; otherwise default package.
if [[ "${CLASS}" != org.* ]]; then
  CLASS="org.postgresql.test.jdbc2.${CLASS}"
fi
EXTRA_FILTERS=("$@")

# Sanity: pgwire reachable.
if ! (exec 3<>/dev/tcp/localhost/15432) 2>/dev/null; then
  echo "ERROR: cannot connect to localhost:15432" >&2
  exit 2
fi
exec 3<&-; exec 3>&-

# Gradle's toolchain auto-detection requires an EXACT languageVersion match
# and pgjdbc hard-codes 17/21 elsewhere, so pin to JDK 21 to avoid the
# GraalVM-25 mismatch ("What went wrong: 25.0.1").
JDK21="${JDK21:-/usr/lib/jvm/java-21-openjdk-amd64}"
if [[ ! -x "${JDK21}/bin/java" ]]; then
  echo "ERROR: JDK 21 not found at ${JDK21}; set JDK21=<path>" >&2
  exit 2
fi

cd "${CLONE_DIR}"
GRADLE_FILTERS=(--tests "${CLASS}")
for f in "${EXTRA_FILTERS[@]}"; do GRADLE_FILTERS+=(--tests "${f}"); done
# Keep the daemon alive between runs — single-test iteration drops from ~30s
# (cold JVM) to ~3-5s (warm daemon). Users of the all-class runner (run.sh)
# still use --no-daemon so CI repeats are deterministic.
JAVA_HOME="${JDK21}" PATH="${JDK21}/bin:${PATH}" \
  ./gradlew :postgresql:test "${GRADLE_FILTERS[@]}" \
  -PjdkBuildVersion=21 \
  --daemon --no-scan \
  -Dorg.gradle.java.home="${JDK21}" > "${LOG}" 2>&1
RC=$?

XML="${CLONE_DIR}/pgjdbc/build/test-results/test/TEST-${CLASS}.xml"
if [[ -f "${XML}" ]]; then
  hdr=$(grep -m1 '<testsuite ' "${XML}")
  t=$(sed -n 's/.*tests="\([0-9]*\)".*/\1/p' <<<"${hdr}" | head -1)
  f=$(sed -n 's/.*failures="\([0-9]*\)".*/\1/p' <<<"${hdr}" | head -1)
  e=$(sed -n 's/.*errors="\([0-9]*\)".*/\1/p' <<<"${hdr}" | head -1)
  s=$(sed -n 's/.*skipped="\([0-9]*\)".*/\1/p' <<<"${hdr}" | head -1)
  t=${t:-0}; f=${f:-0}; e=${e:-0}; s=${s:-0}
  p=$(( t - f - e - s ))
  echo "${CLASS}: ${p} passed, $(( f + e )) failed, ${s} skipped (of ${t})"
else
  echo "${CLASS}: no XML at ${XML}  (gradle rc=${RC})"
  tail -40 "${LOG}"
fi

exit $RC
