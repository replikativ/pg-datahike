# asyncpg wire-protocol regression tests against datahike pgwire

This directory drives a curated subset of the upstream
[asyncpg](https://github.com/MagicStack/asyncpg) test suite against the
Datahike pgwire server.

asyncpg is a useful low-level wire-protocol regression target. It implements
the protocol without libpq, so the tests exercise the bytes on the wire rather
than libpq's compatibility layer. Its prepared-statement tests give us direct
coverage of the Parse, Bind, Describe, and Execute paths.

## Layout

```
asyncpg/
  setup.sh           # clone upstream, create venv, pip install (idempotent)
  run.sh             # run the curated module list, produce a summary
  expected-skips.md  # what we cover, what we skip, and why
  README.md          # this file
  asyncpg/           # (gitignored) cloned upstream source tree
  .venv/             # (gitignored) Python virtualenv with asyncpg + pytest
  last-run.log       # (gitignored) full pytest output from the most recent run
```

## Prerequisites

- Python >= 3.9 on `PATH` (3.12 recommended).
- A C compiler (`cc` / `gcc`) - asyncpg ships Cython extensions that are
  compiled during `pip install -e .`.
- `git`.
- Internet access the first time `setup.sh` runs.
- A running Datahike pgwire server on `localhost:15432`.

`setup.sh` pins asyncpg to `v0.30.0` and pins pytest, pytest-timeout, uvloop,
and distro. It invokes the virtualenv interpreter directly, so moving the
checkout cannot silently fall back to a global Python installation.

## Running

```
./setup.sh     # idempotent: clone + venv + compile C extensions
./run.sh       # run focused modules, ~2-4 min
```

`run.sh` exits 0 when the live failure-ID set exactly matches the checked-in
`expected-failures.txt` manifest. Summary line:

```
SUMMARY: N passed, K failed, M skipped
```

The raw failure count therefore does not determine the exit status. The job
fails if it finds a new failure, a resolved manifest entry, or a manifest entry
that did not run. Parameterized unittest `SUBFAILED` cases are normalized to
their owning pytest test ID before comparison. `last-run.log` has the full
pytest output.

## What is covered

See `expected-skips.md`. In short: extended-query protocol
(Parse/Bind/Describe/Execute/Sync), connection startup, prepared statement
caching, cursor/portal management, transactions, error mapping, per-type
codecs. COPY, LISTEN/NOTIFY, connection-pool lifecycle, timeout races, and
chaos tests remain separate compatibility tranches.

## Environment variables used

`run.sh` exports these before invoking pytest. asyncpg's testbase detects
`PGHOST` and switches to "use the already-running cluster" mode rather than
trying to spawn `initdb`.

| Var | Value |
|---|---|
| `PGHOST` | `127.0.0.1` |
| `PGPORT` | `15432` |
| `PGUSER` | `datahike` |
| `PGDATABASE` | `datahike` |
| `PGPASSWORD` | `datahike` |
| `PGSSLMODE` | `disable` |

## Interpreting output

- no drift section: the live failures exactly match the manifest.
- `REGRESSION: ... failing that are NOT in expected-failures.txt`: a new
  compatibility failure; `last-run.log` has the full stack.
- `BASELINE DRIFT: ... now PASS`: coverage improved; prune the manifest. The
  job fails until the checked-in baseline is updated deliberately.
- `COVERAGE HOLE: ... DID NOT RUN`: the test was renamed, deselected, or its
  module stopped early. The job fails because this is not evidence of a fix.
- pytest rc != 0 with zero failures reported: likely a collection/import
  error in asyncpg itself. Look for `ERROR` lines early in the log.

## Timing

A focused run takes about 2-4 minutes and needs a freshly started in-memory
server. Failed cleanup can otherwise create misleading cascades in later
modules. The full upstream suite takes about 10 minutes and remains outside
this gate's scope.
