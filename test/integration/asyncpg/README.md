# asyncpg wire-protocol regression tests against datahike pgwire

This directory drives a curated subset of the upstream
[asyncpg](https://github.com/MagicStack/asyncpg) test suite against the
Datahike pgwire server.

asyncpg is the cleanest low-level wire-protocol regression target available:
it is a pure-Python protocol implementation with no libpq dependency, so its
tests exercise the actual bytes on the wire rather than libpq's compatibility
layer. When asyncpg's `test_prepare.py` fails, our Parse/Bind/Describe path
has a bug, full stop.

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

`setup.sh` pins asyncpg to `v0.30.0`.

## Running

```
./setup.sh     # idempotent: clone + venv + compile C extensions
./run.sh       # run focused modules, ~1-3 min
```

`run.sh` exits 0 when every live failure is present in the checked-in
`expected-failures.txt` manifest. Summary line:

```
SUMMARY: 99 passed, 70 failed, 32 skipped
```

The raw failure count therefore does not determine the exit status. A failure
outside the manifest is a regression and fails the job; a manifest entry that
now passes is reported as resolved and should be removed. `last-run.log` has
the full pytest output.

## What is covered

See `expected-skips.md`. In short: extended-query protocol
(Parse/Bind/Describe/Execute/Sync), connection startup, prepared statement
caching, cursor/portal management, transactions, error mapping, per-type
codecs. Explicitly excluded: COPY, LISTEN/NOTIFY, pool-with-cancel,
logical-replication adjacent tests.

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

- no `REGRESSION` section: the live failures exactly match the manifest.
- `REGRESSION: ... failing that are NOT in expected-failures.txt`: a new
  compatibility failure; `last-run.log` has the full stack.
- `expected-failure(s) now PASS`: coverage improved; prune the manifest.
- `expected-failure(s) DID NOT RUN`: the test was renamed, deselected, or its
  module stopped early; this is a coverage hole, not a fix.
- pytest rc != 0 with zero failures reported: likely a collection/import
  error in asyncpg itself. Look for `ERROR` lines early in the log.

## Timing

Single focused run is ~1-3 minutes. The full upstream suite takes ~10 minutes
and exercises features we do not implement - don't run it.
