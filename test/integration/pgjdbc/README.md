# pgjdbc conformance tests against datahike pgwire

This directory drives a curated subset of the upstream
[pgjdbc](https://github.com/pgjdbc/pgjdbc) test suite against the Datahike
pgwire server. It is the highest-signal external suite we run, because
CockroachDB and YugabyteDB both use pgjdbc as their PostgreSQL-compatibility
gate.

## Layout

```
pgjdbc/
  setup.sh           # clone upstream + write build.local.properties (idempotent)
  run.sh             # run the curated focus list, produce a summary
  expected-skips.md  # what we cover, what we skip, and why
  README.md          # this file
  pgjdbc/            # (gitignored) cloned upstream source tree
  last-run.log       # (gitignored) full gradle output from the most recent run
```

## Prerequisites

- Java >= 17 on `PATH`. The pinned pgjdbc build supports Java through 23;
  `run.sh` falls back to a system JDK 21 when the active Java is newer.
- `git`.
- Internet access the first time `setup.sh` runs.
- A running Datahike pgwire server on `localhost:15432`, started from the
  repo root:
  ```
  clojure -M:server
  ```

`setup.sh` and `run.sh` do not start or stop the server.

## Running

```
./setup.sh     # idempotent: clones pgjdbc@REL42.7.5 + writes local props
./run.sh       # runs the focus list, ~3-10 min depending on hardware
```

`run.sh` exits 0 when every non-skipped test passes, and non-zero otherwise.
The summary line looks like:

```
SUMMARY: 275 passed, 0 failed, 1 skipped   (gradle rc=0)
```

`last-run.log` captures the full gradle output for triage.

## What is covered

See `expected-skips.md`. In short: driver and connection behavior,
server-prepared statements, ResultSet and batch execution, selected JDBC 4.2
parameters, and metadata properties. Classes dominated by stored functions,
generated identities, custom types, database-wide settings or unsupported
protocol subsystems remain measured but deferred.

## Interpreting output

- `N passed, 0 failed, M skipped`: clean run. `M > 0` is fine - the upstream
  suite has JUnit-level `@Disabled` tests for server versions etc.
- `N passed, K failed, M skipped` with `K > 0`:
  - Cross-check every failing test against `expected-skips.md`. If it is
    listed under "Expected failures", that is not a regression.
  - Otherwise: a real regression. `last-run.log` has the full stack.
- Gradle rc != 0 with zero test failures reported: usually a build / daemon /
  checkstyle problem, not a server bug. `run.sh` disables checkstyle,
  spotbugs, and forbiddenApis to minimize this.

## Timing

A single focused run takes 3-10 minutes wall-clock on a modern laptop (most
of that is Gradle JVM warmup and test isolation). The full upstream suite
takes 30+ minutes and exercises features we do not implement - don't run it.

## Adding / removing tests

- Edit the `TESTS=(...)` array in `run.sh`.
- Add or remove the corresponding entry in `expected-skips.md`.
- Keep the list narrow. The whole point is high signal per minute.
