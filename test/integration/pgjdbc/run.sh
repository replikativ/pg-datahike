#!/usr/bin/env bash
# Run a curated subset of the pgjdbc test suite against Datahike pgwire.
#
# Requirements:
#   - setup.sh has been run (pgjdbc/ exists, build.local.properties is in place)
#   - Datahike pgwire server is listening on localhost:15432
#
# Exit code: 0 if every non-skipped test passed, non-zero otherwise.
#
# Output: a last-run.log with full gradle output is written in this directory.
# Final summary line: "N passed, K failed, M skipped".
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLONE_DIR="${HERE}/pgjdbc"
LOG="${HERE}/last-run.log"

if [[ ! -d "${CLONE_DIR}" ]]; then
  echo "ERROR: pgjdbc/ is missing. Run ./setup.sh first." >&2
  exit 2
fi

# Sanity check: pgwire server reachable.
if ! (exec 3<>/dev/tcp/localhost/15432) 2>/dev/null; then
  echo "ERROR: cannot connect to localhost:15432 - is the pgwire server running?" >&2
  echo "  cd \$DATAHIKE && DATAHIKE_QUERY_PLANNER=true clojure -A:test -M test/integration/start_pgwire.clj" >&2
  exit 2
fi
exec 3<&-; exec 3>&-

# --- focus list --------------------------------------------------------------
#
# Gradle --tests accepts one or more fully-qualified class names (wildcards ok).
# The list below is intentionally narrow: core JDBC surface that we expect to
# work, with deliberate exclusions captured in expected-skips.md.
#
# If you add a class here, add a one-line rationale in expected-skips.md under
# the "included" section.
TESTS=(
  # --- Verified-stable JDBC surface.
  # The rest of the pgjdbc class list
  # (DriverTest, ConnectionTest, PreparedStatementTest,
  # DatabaseMetaData*, …) is in active development — see
  # `expected-skips.md → Deferred classes` for the specific gaps. When
  # those land, re-add the class name here one at a time and re-run.
  "org.postgresql.test.jdbc2.ResultSetTest"
  "org.postgresql.test.jdbc2.BatchExecuteTest"
)

# Build the --tests argument list for gradle.
GRADLE_ARGS=(":postgresql:test")
for t in "${TESTS[@]}"; do
  GRADLE_ARGS+=("--tests" "${t}")
done

# Override pgjdbc's hard-coded toolchain language version (default 17,
# from build-logic/build-parameters). Gradle's auto-detection requires
# an exact match for the requested languageVersion; without an override,
# a JDK 21 dev box fails with "No locally installed toolchains match."
#
# pgjdbc's Gradle build rejects very new JDKs — REL42.7.5's build-logic
# tops out around Java 23. If `java` on PATH is >= 24, fall back to
# JDK 21 (the pinned-in-CI version per doc/integration-testing.md) when
# we can find it on disk. Override via PGJDBC_JDK_HOME if you'd rather
# point at a specific install.
JAVA_HOME_OVERRIDE="${PGJDBC_JDK_HOME:-}"
if [[ -z "${JAVA_HOME_OVERRIDE}" ]]; then
  CUR_MAJOR="$(java -version 2>&1 | awk -F '[".]' '/version/ {v=$2; if (v=="1") v=$3; print v; exit}')"
  if [[ -n "${CUR_MAJOR}" && "${CUR_MAJOR}" -ge 24 ]]; then
    for candidate in \
      "/usr/lib/jvm/java-21-openjdk-amd64" \
      "/usr/lib/jvm/temurin-21-jdk-amd64" \
      "/Library/Java/JavaVirtualMachines/temurin-21.jdk/Contents/Home" ; do
      if [[ -x "${candidate}/bin/javac" ]]; then
        JAVA_HOME_OVERRIDE="${candidate}"
        echo "[run] current java is ${CUR_MAJOR}, too new for pgjdbc; using ${JAVA_HOME_OVERRIDE}"
        break
      fi
    done
  fi
fi
if [[ -n "${JAVA_HOME_OVERRIDE}" ]]; then
  export JAVA_HOME="${JAVA_HOME_OVERRIDE}"
  export PATH="${JAVA_HOME}/bin:${PATH}"
fi
JAVA_MAJOR="$(java -version 2>&1 | awk -F '[".]' '/version/ {v=$2; if (v=="1") v=$3; print v; exit}')"
if [[ -z "${JAVA_MAJOR}" ]]; then JAVA_MAJOR=17; fi
GRADLE_ARGS+=("-PjdkBuildVersion=${JAVA_MAJOR}")

GRADLE_ARGS+=(
  "--no-daemon"
  "--no-scan"
  "--continue"       # don't stop at first failing class
)

echo "[run] cd ${CLONE_DIR}"
echo "[run] ./gradlew ${GRADLE_ARGS[*]}"
echo "[run] full output -> ${LOG}"

cd "${CLONE_DIR}"
# tee the gradle output so CI stdout sees progress during the ~6-10m
# run — a plain redirect starves CircleCI's no_output_timeout.
set -o pipefail
# shellcheck disable=SC2068
./gradlew ${GRADLE_ARGS[@]} --console=plain 2>&1 | tee "${LOG}"
GRADLE_RC=${PIPESTATUS[0]}

# --- summarize ---------------------------------------------------------------
#
# Parse each per-class JUnit XML report under pgjdbc/build/test-results/test/.
# Each <testsuite ...> has tests/failures/errors/skipped attributes.
REPORTS_DIR="${CLONE_DIR}/pgjdbc/build/test-results/test"
P=0; F=0; S=0; E=0
if [[ -d "${REPORTS_DIR}" ]]; then
  while IFS= read -r xml; do
    # Grab the first <testsuite ...> tag's counts.
    hdr="$(grep -m1 '<testsuite ' "${xml}" || true)"
    tests=$(   sed -n 's/.*tests="\([0-9]*\)".*/\1/p'    <<<"${hdr}" | head -1)
    fails=$(   sed -n 's/.*failures="\([0-9]*\)".*/\1/p' <<<"${hdr}" | head -1)
    errors=$(  sed -n 's/.*errors="\([0-9]*\)".*/\1/p'   <<<"${hdr}" | head -1)
    skipped=$( sed -n 's/.*skipped="\([0-9]*\)".*/\1/p'  <<<"${hdr}" | head -1)
    tests=${tests:-0}; fails=${fails:-0}; errors=${errors:-0}; skipped=${skipped:-0}
    P=$(( P + tests - fails - errors - skipped ))
    F=$(( F + fails ))
    E=$(( E + errors ))
    S=$(( S + skipped ))
  done < <(find "${REPORTS_DIR}" -name 'TEST-*.xml' 2>/dev/null)
else
  echo "[run] WARN: no reports dir at ${REPORTS_DIR}" >&2
fi

FAIL_TOTAL=$(( F + E ))
echo
echo "SUMMARY: ${P} passed, ${FAIL_TOTAL} failed, ${S} skipped   (gradle rc=${GRADLE_RC})"

if [[ ${GRADLE_RC} -ne 0 ]] || [[ ${FAIL_TOTAL} -gt 0 ]]; then
  # Show a tail of the log to help triage without re-running.
  echo
  echo "--- last 60 lines of ${LOG} ---"
  tail -n 60 "${LOG}" || true
  exit 1
fi
exit 0
